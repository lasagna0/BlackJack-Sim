# blackjack_hand_logic.r - Core Blackjack Hand Gameplay Logic

# Get card value for gameplay
card_value <- function(card) {
  if (card %in% c("J", "Q", "K")) return(10)
  else if (card == "A") return(11) # Aces are initially 11
  else return(as.numeric(card))
}

# Count value for Hi-Lo system
count_value <- function(card) {
  if (card %in% c("2", "3", "4", "5", "6")) return(1)
  else if (card %in% c("10", "J", "Q", "K", "A")) return(-1)
  else return(0)
}

# Calculate hand value (total points)
hand_value <- function(hand) {
  if (length(hand) == 0) return(0)
  values <- sapply(hand, card_value)
  total <- sum(values)
  aces <- sum(hand == "A")
  while (total > 21 && aces > 0) {
    total <- total - 10  # Count Ace as 1 instead of 11
    aces <- aces - 1
  }
  return(total)
}

# Get detailed attributes of a hand
get_hand_details <- function(hand) {
  if (length(hand) == 0) {
    return(list(value = 0, is_soft = FALSE, is_blackjack = FALSE, num_aces = 0))
  }
  
  current_value <- hand_value(hand)
  is_blackjack <- (length(hand) == 2 && current_value == 21)
  num_aces <- sum(hand == "A")
  is_soft <- FALSE
  
  if (num_aces > 0) {
    # Calculate value if all Aces in this hand were 1.
    val_aces_as_one <- 0
    for(card_in_hand in hand){
        if(card_in_hand == "A") val_aces_as_one <- val_aces_as_one + 1
        else val_aces_as_one <- val_aces_as_one + card_value(card_in_hand)
    }
    # If current_value (optimal) is 10 more than val_aces_as_one, an Ace is being counted as 11.
    # And current_value must be <= 21 for this soft count to be valid.
    if (val_aces_as_one + 10 == current_value && current_value <= 21) {
        is_soft <- TRUE
    }
  }
  return(list(value = current_value, is_soft = is_soft, is_blackjack = is_blackjack, num_aces = num_aces))
}

# Simulate a hand (Main logic for playing one round)
play_hand <- function(shoe, count, initial_bet = 10, strategy_fn, game_rules, current_true_count) {
  
  dealer_hand_for_log_initial <- c()
  dealer_hand_for_log_final <- c()

  if (length(shoe) < 4) { 
    warning("Shoe too depleted for initial deal.")
    return(list(shoe = shoe, count = count, profit = 0, hands_played_details = list(), insurance_profit = 0, 
                dealer_hand_initial = dealer_hand_for_log_initial, dealer_hand_final = dealer_hand_for_log_final))
  }
  player_initial_hand <- shoe[1:2]
  dealer_hand_initial <- shoe[3:4] 
  dealer_hand_for_log_initial <- dealer_hand_initial # Log initial dealer hand
  shoe <- shoe[-(1:4)]
  
  cards_for_initial_count <- c(player_initial_hand, dealer_hand_initial)
  
  dealer_up_card <- dealer_hand_initial[1]
  current_dealer_hand <- dealer_hand_initial 
  total_profit <- 0 # This will be the sum of main bet profit and insurance profit for the round
  main_bet_total_profit <- 0 # Profit specifically from main game bets (excluding insurance)
  insurance_profit <- 0
  insurance_bet_taken <- 0
  took_insurance <- FALSE

  player_hands <- list(list(cards = player_initial_hand, bet = initial_bet, status = "active", times_split = 0, original_hand = TRUE))
  hands_final_outcomes <- list()

  # --- Insurance Phase ---
  dealer_shows_ace <- (card_value(dealer_up_card) == 11)
  if (dealer_shows_ace && game_rules$allow_insurance && initial_bet > 0) {
    # Strategy function needs to handle a query_type = "insurance_decision"
    # It should return a list, e.g., list(take_insurance = TRUE/FALSE)
    # For now, full_basic_strategy (and others) need to be updated to handle this.
    # Let's assume it returns a simple TRUE/FALSE for this query for now or a list with $take_insurance
    possible_insurance_action <- list(query_type = "insurance_decision", can_take_insurance = TRUE) # Let strategy know it can decide
    insurance_decision <- strategy_fn(player_initial_hand, dealer_up_card, game_rules, possible_insurance_action, current_true_count)
    
    if (is.list(insurance_decision) && !is.null(insurance_decision$take_insurance) && insurance_decision$take_insurance) {
      took_insurance <- TRUE
      insurance_bet_taken <- initial_bet / 2 
    } else if (is.logical(insurance_decision) && insurance_decision == TRUE) { # Simpler TRUE/FALSE return
      took_insurance <- TRUE
      insurance_bet_taken <- initial_bet / 2 
    }
  }
  # --- End Insurance Phase ---

  # Update running count with initially dealt cards (player & dealer)
  for (card_val in cards_for_initial_count) { count <- count + count_value(card_val) }
  
  dealer_initial_details <- get_hand_details(dealer_hand_initial)
  dealer_has_blackjack_initially <- dealer_initial_details$is_blackjack

  if (took_insurance) {
    if (dealer_has_blackjack_initially) {
      insurance_profit <- insurance_bet_taken * 2 
    } else {
      insurance_profit <- -insurance_bet_taken 
    }
  }

  player_initial_details <- get_hand_details(player_initial_hand)

  if (player_initial_details$is_blackjack) {
    if (dealer_has_blackjack_initially) { # Player BJ, Dealer BJ
      # Main bet is a push (0 profit)
    } else { # Player BJ, Dealer no BJ
      main_bet_total_profit <- main_bet_total_profit + (initial_bet * game_rules$blackjack_payout)
    }
    hands_final_outcomes[[1]] <- list(hand = player_initial_hand, bet = initial_bet, outcome_profit = main_bet_total_profit, final_value = player_initial_details$value, status="blackjack")
  } else if (dealer_has_blackjack_initially) { # Player no BJ, Dealer BJ
      main_bet_total_profit <- main_bet_total_profit - initial_bet 
      hands_final_outcomes[[1]] <- list(hand = player_initial_hand, bet = initial_bet, outcome_profit = -initial_bet, final_value = player_initial_details$value, status="lost_to_dealer_bj")
  } else {
    # Neither player nor dealer has initial Blackjack. Player's turn.
    current_hand_idx <- 1
    while(current_hand_idx <= length(player_hands)) {
      current_player_hand_info <- player_hands[[current_hand_idx]]
      
      if (current_player_hand_info$status != "active") { # Already stood, busted, or voided
        current_hand_idx <- current_hand_idx + 1
        next
      }
      
      current_player_cards <- current_player_hand_info$cards
      current_player_bet <- current_player_hand_info$bet 
      player_times_split <- current_player_hand_info$times_split

      player_turn_active_for_this_hand <- TRUE
      while(player_turn_active_for_this_hand) {
        player_details <- get_hand_details(current_player_cards)
        if (player_details$value >= 21) { # Player busts or hits 21 (auto-stand)
          player_hands[[current_hand_idx]]$status <- if(player_details$value > 21) "busted" else "stood"
          player_hands[[current_hand_idx]]$cards <- current_player_cards # Save updated cards
          player_turn_active_for_this_hand <- FALSE 
          break 
        }

        possible_actions <- list(can_hit = TRUE, can_stand = TRUE, can_double = FALSE, can_split = FALSE, can_surrender = FALSE, query_type = "action")
        
        # Surrender (only for original hand, first action, if dealer doesn't have BJ - already checked)
        if (current_player_hand_info$original_hand && length(current_player_cards) == 2 && game_rules$allow_surrender == "Late") {
            if (!(game_rules$surrender_restrict_on_dealer_ace && card_value(dealer_up_card) == 11)) {
                possible_actions$can_surrender <- TRUE
            }
        }
        # Split
        is_pair <- length(current_player_cards) == 2 && (card_value(current_player_cards[1]) == card_value(current_player_cards[2]))
        if (game_rules$allow_split && is_pair && player_times_split < game_rules$max_resplits) {
            if (card_value(current_player_cards[1]) == 11 && player_times_split > 0 && !game_rules$resplit_aces_allowed) {
                # Cannot resplit Aces if rule disallows and it's not the first split of Aces
            } else {
                 possible_actions$can_split <- TRUE
            }
        }
        # Double
        if (game_rules$allow_double && length(current_player_cards) == 2) {
          is_original_hand_or_das_allowed <- current_player_hand_info$original_hand || game_rules$double_after_split
          if (is_original_hand_or_das_allowed && 
             (game_rules$double_on_any_two_cards || player_details$value %in% game_rules$double_on_totals)) {
             possible_actions$can_double <- TRUE
          }
        }
        
        action <- strategy_fn(current_player_cards, dealer_up_card, game_rules, possible_actions, current_true_count)

        if (action == "surrender" && possible_actions$can_surrender) {
            hand_profit_for_this <- -(current_player_bet / 2)
            main_bet_total_profit <- main_bet_total_profit + hand_profit_for_this 
            player_hands[[current_hand_idx]]$status <- "surrendered"
            player_hands[[current_hand_idx]]$outcome_profit <- hand_profit_for_this # Store profit with hand
            # hands_final_outcomes logic will pick this up later
            player_turn_active_for_this_hand <- FALSE
            if(current_player_hand_info$original_hand) { # If original hand surrenders, game ends for player bets
                 for(k_hand_idx in seq_along(player_hands)) player_hands[[k_hand_idx]]$status <- "void_surrender"
            }
            break 
        } else if (action == "split" && possible_actions$can_split) {
            if (length(shoe) < 2) { 
                warning("Shoe ran out attempting to split. Defaulting to hit.")
                action <- "hit" 
            } else {
                card1_val <- current_player_cards[1] # The actual card string
                card2_val <- current_player_cards[2] # The actual card string
                
                # Prepare new hands (info only, cards dealt next)
                hand1_info <- list(cards=c(card1_val), bet=current_player_bet, status="active", times_split=player_times_split+1, original_hand=FALSE)
                hand2_info <- list(cards=c(card2_val), bet=current_player_bet, status="active", times_split=player_times_split+1, original_hand=FALSE)

                # Deal second card to first new hand
                new_card_h1 <- shoe[1]; shoe <- shoe[-1]; count <- count + count_value(new_card_h1)
                hand1_info$cards <- c(hand1_info$cards, new_card_h1)
                
                # Deal second card to second new hand
                new_card_h2 <- shoe[1]; shoe <- shoe[-1]; count <- count + count_value(new_card_h2)
                hand2_info$cards <- c(hand2_info$cards, new_card_h2)

                # Ace split rule: stand if not allowed to hit split aces
                if (card_value(card1_val) == 11 && !game_rules$hit_split_aces_allowed) hand1_info$status <- "stood"
                if (card_value(card2_val) == 11 && !game_rules$hit_split_aces_allowed) hand2_info$status <- "stood"
                
                # Reconstruct player_hands list
                first_part <- if(current_hand_idx > 1) player_hands[1:(current_hand_idx-1)] else list()
                remaining_part <- if(current_hand_idx < length(player_hands)) player_hands[(current_hand_idx+1):length(player_hands)] else list()
                player_hands <- c(first_part, list(hand1_info, hand2_info), remaining_part)
                
                # Current hand becomes the first of the split hands. Loop will continue for it.
                current_player_cards <- player_hands[[current_hand_idx]]$cards 
                # If the new current hand is already "stood" (e.g. split Aces), end its turn
                if (player_hands[[current_hand_idx]]$status != "active") {
                    player_turn_active_for_this_hand <- FALSE
                } # else, loop continues for this new hand
                next # Restart inner loop for the newly formed current_player_cards (hand1_info)
            }
        } else if (action == "double" && possible_actions$can_double) {
            if (length(shoe) < 1) { 
                warning("Shoe ran out attempting to double. Defaulting to hit.")
                action <- "hit"
            } else {
                doubled_bet_for_this_hand <- current_player_bet * 2
                card <- shoe[1]; shoe <- shoe[-1]; count <- count + count_value(card)
                current_player_cards <- c(current_player_cards, card)
                player_hands[[current_hand_idx]]$cards <- current_player_cards
                player_hands[[current_hand_idx]]$bet <- doubled_bet_for_this_hand # Update bet for this hand
                player_hands[[current_hand_idx]]$status <- "stood" 
                player_turn_active_for_this_hand <- FALSE # Turn ends for this hand
            }
        } 
        
        # This 'if' block must come AFTER double/split/surrender attempts that might default to 'hit'
        if (action == "hit") { 
            if (length(shoe) < 1) { 
                warning("Shoe ran out on hit.")
                player_hands[[current_hand_idx]]$status <- "stood" # Force stand if cannot hit
                player_turn_active_for_this_hand <- FALSE; break 
            }
            card <- shoe[1]; shoe <- shoe[-1]; count <- count + count_value(card)
            current_player_cards <- c(current_player_cards, card)
            player_hands[[current_hand_idx]]$cards <- current_player_cards
            # Loop continues, player_details re-evaluated at top
        } else if (action == "stand") {
            player_hands[[current_hand_idx]]$status <- "stood"
            player_turn_active_for_this_hand <- FALSE
        } else if (action %in% c("double", "split", "surrender")) {
            # Already handled by specific blocks or loop broken/continued
            if(!player_turn_active_for_this_hand) break # Ensure exit if action ended turn
        } else { 
             warning(paste("Unknown action from strategy:", action, "Player hand:", paste(current_player_cards, collapse=","), "Defaulting to stand."))
             player_hands[[current_hand_idx]]$status <- "stood"
             player_turn_active_for_this_hand <- FALSE
        }
      } # end while player_turn_active_for_this_hand
      current_hand_idx <- current_hand_idx + 1
    } # end while current_hand_idx <= length(player_hands) (loop through all player hands)

    # Dealer's turn (only if game not ended by initial BJs & player has live hands)
    dealer_plays <- FALSE
    # Check if any hand is still in play (not busted or surrendered)
    player_has_live_hands <- any(sapply(player_hands, function(h) h$status == "stood" || h$status == "active")) # Active means stood due to e.g. split Ace no hit
    if (player_has_live_hands) {
        dealer_plays <- TRUE
    }
    
    dealer_final_details_val <- get_hand_details(current_dealer_hand) 
    if (dealer_plays) {
        while (dealer_final_details_val$value < 17 || (dealer_final_details_val$value == 17 && dealer_final_details_val$is_soft && game_rules$dealer_hits_soft_17)) {
            if (length(shoe) < 1) { warning("Shoe ran out during dealer hit"); break }
            card <- shoe[1]; shoe <- shoe[-1]; count <- count + count_value(card)
            current_dealer_hand <- c(current_dealer_hand, card)
            dealer_final_details_val <- get_hand_details(current_dealer_hand)
        }
    }
    dealer_hand_for_log_final <- current_dealer_hand # Log final dealer hand
    dealer_final_player_view_details <- get_hand_details(current_dealer_hand) # Dealer's final hand state

    # Resolve outcomes for hands that were not initial BJs and not surrendered
    for (i in 1:length(player_hands)) {
      hand_info <- player_hands[[i]]
      # Skip if already resolved (like surrender) or voided
      if (hand_info$status == "surrendered" || hand_info$status == "void_surrender" || hand_info$status == "blackjack" || hand_info$status == "lost_to_dealer_bj") {
          # If it was a BJ/lost_to_dealer_bj, its profit is already in main_bet_total_profit
          # If surrendered, its profit is also already in main_bet_total_profit
          # Add to hands_final_outcomes if not already (e.g. surrender added it)
          is_already_logged = any(sapply(hands_final_outcomes, function(hfo) identical(hfo$hand, hand_info$cards) && hfo$bet == hand_info$bet))
          if(!is_already_logged && (hand_info$status == "surrendered" || hand_info$status == "blackjack" || hand_info$status == "lost_to_dealer_bj")) {
             hands_final_outcomes[[length(hands_final_outcomes)+1]] <- list(
                hand = hand_info$cards, bet = hand_info$bet, 
                outcome_profit = hand_info$outcome_profit %||% 0, # Use stored profit if available
                final_value = get_hand_details(hand_info$cards)$value, status = hand_info$status)
          }
          next 
      }
      
      p_cards <- hand_info$cards
      p_bet <- hand_info$bet 
      p_details <- get_hand_details(p_cards) # Final details of this player hand
      hand_outcome_profit = 0
      
      is_player_bj_on_split_not_original = p_details$is_blackjack && !hand_info$original_hand && game_rules$blackjack_after_split_counts_as_21

      if (hand_info$status == "busted") { 
        hand_outcome_profit <- -p_bet
      } else if (hand_info$status == "stood" || hand_info$status == "active") { # Active here means stood (e.g. split aces)
        if (dealer_final_player_view_details$is_blackjack && !p_details$is_blackjack) { # Dealer natural BJ beats player's non-BJ stood hand
            hand_outcome_profit <- -p_bet
        } else if (dealer_final_player_view_details$value > 21) { # Dealer busts
            hand_outcome_profit <- p_bet
        } else if (p_details$value > dealer_final_player_view_details$value) { # Player wins
            hand_outcome_profit <- p_bet
        } else if (p_details$value < dealer_final_player_view_details$value) { # Dealer wins
            hand_outcome_profit <- -p_bet
        } else { # Push
            hand_outcome_profit <- 0
        }
      } # End stood/active hand resolution
      main_bet_total_profit <- main_bet_total_profit + hand_outcome_profit
      hands_final_outcomes[[length(hands_final_outcomes)+1]] <- list(hand = p_cards, bet = p_bet, outcome_profit = hand_outcome_profit, final_value = p_details$value, status = hand_info$status)
    }
  } # End of if/else for initial Player/Dealer Blackjack check (main game play path)
  
  total_profit <- main_bet_total_profit + insurance_profit

  return(list(shoe = shoe, count = count, profit = total_profit, 
              hands_played_details = hands_final_outcomes, 
              insurance_profit = insurance_profit,
              dealer_hand_initial = dealer_hand_for_log_initial, 
              dealer_hand_final = dealer_hand_for_log_final))  
}

# Helper for null coalescing (used for outcome_profit in logging)
`%||%` <- function(a, b) if (!is.null(a)) a else b 