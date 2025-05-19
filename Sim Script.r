set.seed(21)

# Default Game Rules
default_game_rules <- list(
  num_decks = 6,
  dealer_hits_soft_17 = TRUE, # TRUE for S17, FALSE for H17
  blackjack_payout = 1.5,     # 3:2 payout for Blackjack
  penetration_percentage = 0.75, # Proportion of shoe dealt before reshuffle (e.g., 0.75 = 75%)
  allow_double = TRUE,                # General toggle for doubling
  double_on_any_two_cards = FALSE,    # If TRUE, double_on_totals is ignored for 2 cards
  double_on_totals = c(9, 10, 11),  # Specific two-card totals player can double on
  allow_split = TRUE,                 # General toggle for splitting
  max_resplits = 3,                   # Max number of splits (e.g., 3 = up to 4 hands total)
  resplit_aces_allowed = FALSE,       # Can aces be resplit?
  hit_split_aces_allowed = FALSE,     # Can player hit after splitting aces (often only one card per ace)
  double_after_split = TRUE,          # Can player double down after splitting a pair?
  blackjack_after_split_counts_as_21 = TRUE, # If TRUE, A+10 on a split hand is just 21, not Blackjack payout
  allow_surrender = FALSE             # Placeholder for future use
  # Future rules: allow_double_after_split, allow_resplit_aces, etc.
)
 
# Create and shuffle a labeled shoe
create_shoe <- function(game_rules) {
  ranks <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A")
  shoe <- rep(ranks, each = 4 * game_rules$num_decks)
  return(sample(shoe))
}

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
# This is the core logic for determining the numeric value of a hand.
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
    # A hand is soft if at least one Ace is counted as 11.
    # Calculate value if all Aces in this hand were 1.
    value_if_all_aces_were_one <- 0
    non_ace_value <- 0
    if(any(hand != "A")) { # Check if there are non-ace cards
        non_ace_value <- sum(sapply(hand[hand != "A"], card_value))
    }
    value_if_all_aces_were_one <- non_ace_value + num_aces # Each Ace as 1
    
    # If current_value is 10 more than value_if_all_aces_were_one,
    # it means one Ace is being counted as 11 (10 more than 1).
    if (current_value == value_if_all_aces_were_one + 10) {
      is_soft <- TRUE
    }
  }
  
  return(list(value = current_value, is_soft = is_soft, is_blackjack = is_blackjack))
}

# Basic strategy placeholder: just hits below 17
basic_strategy <- function(player_hand, dealer_up_card, game_rules, possible_actions) {
  player_details <- get_hand_details(player_hand)
  if (player_details$value >= 17) return("stand")
  else return("hit")
}

# New Strategy: Never Bust (Stand on 12+)
never_bust_strategy <- function(player_hand, dealer_up_card, game_rules, possible_actions) {
  player_details <- get_hand_details(player_hand)
  if (player_details$value >= 12) return("stand")
  else return("hit")
}

# New Strategy: Mimic Dealer
mimic_dealer_strategy <- function(player_hand, dealer_up_card, game_rules, possible_actions) {
  player_details <- get_hand_details(player_hand)
  # Mimics H17 rule by default, or S17 if game_rules$dealer_hits_soft_17
  if (player_details$value > 17) return("stand")
  if (player_details$value == 17 && !player_details$is_soft) return("stand")
  if (player_details$value == 17 && player_details$is_soft && !game_rules$dealer_hits_soft_17) return("stand") # Player stands on soft 17 if dealer would
  else return("hit")
}

# Poor Double strategy: very naively doubles, no split logic yet
poor_double_strategy <- function(player_hand, dealer_up_card, game_rules, possible_actions) {
  player_details <- get_hand_details(player_hand)
  if (possible_actions$can_double && player_details$value %in% c(10,11)) {
    return("double")
  } else if (player_details$value >= 17) {
    return("stand")
  } else {
    return("hit")
  }
}

# Simulate a hand
play_hand <- function(shoe, count, initial_bet = 10, strategy_fn, game_rules) {
  
  # Initial deal
  if (length(shoe) < 4) { 
    warning("Shoe too depleted for initial deal.")
    return(list(shoe = shoe, count = count, profit = 0, hands_played_details = list()))
  }
  player_initial_hand <- shoe[1:2]
  dealer_hand <- shoe[3:4]
  shoe <- shoe[-(1:4)]
  for (card in c(player_initial_hand, dealer_hand)) { count <- count + count_value(card) }

  dealer_up_card <- dealer_hand[1]
  total_profit <- 0

  # List to manage player's hands. Each element is a list: list(cards = hand, bet = bet, status = "active"/"stood"/"busted", times_split = 0)
  player_hands <- list(list(cards = player_initial_hand, bet = initial_bet, status = "active", times_split = 0, original_hand = TRUE))
  hands_final_outcomes <- list() # To store final details of each hand for record-keeping

  # Check for initial player Blackjack (only on the original hand before any splits)
  player_initial_details <- get_hand_details(player_initial_hand)
  dealer_initial_details <- get_hand_details(dealer_hand) # Get dealer details early for BJ checks

  if (player_initial_details$is_blackjack) {
    if (dealer_initial_details$is_blackjack) {
      total_profit <- 0 # Push
    } else {
      total_profit <- initial_bet * game_rules$blackjack_payout
    }
    hands_final_outcomes[[1]] <- list(hand = player_initial_hand, bet = initial_bet, outcome_profit = total_profit, final_value = player_initial_details$value, status="blackjack")
    # If player has BJ, game ends here for this round from player's perspective, no further actions.
    # Dealer will still play out their hand if they don't have BJ, for card counting purposes below for the dealer's turn.
  } else if (dealer_initial_details$is_blackjack) {
      total_profit <- -initial_bet # Player loses initial bet, no chance to act
      hands_final_outcomes[[1]] <- list(hand = player_initial_hand, bet = initial_bet, outcome_profit = total_profit, final_value = player_initial_details$value, status="lost_to_dealer_bj")
      # Game ends here if dealer has BJ.
  } else {
    # Player does not have BJ, dealer does not have BJ. Proceed with playing hands.
    current_hand_idx <- 1
    while(current_hand_idx <= length(player_hands)) {
      current_player_hand_info <- player_hands[[current_hand_idx]]
      current_player_cards <- current_player_hand_info$cards
      current_player_bet <- current_player_hand_info$bet
      player_hand_status <- current_player_hand_info$status
      player_times_split <- current_player_hand_info$times_split

      if (player_hand_status != "active") { # Hand already stood or busted (e.g. from split aces rule)
        current_hand_idx <- current_hand_idx + 1
        next
      }

      player_turn_active_for_this_hand <- TRUE
      while(player_turn_active_for_this_hand) {
        player_details <- get_hand_details(current_player_cards)

        if (player_details$value >= 21) { # Bust or 21
          player_turn_active_for_this_hand <- FALSE
          player_hands[[current_hand_idx]]$status <- if(player_details$value > 21) "busted" else "stood"
          player_hands[[current_hand_idx]]$cards <- current_player_cards # Save updated cards
          break
        }

        possible_actions <- list(can_hit = TRUE, can_stand = TRUE, can_double = FALSE, can_split = FALSE)
        is_pair <- length(current_player_cards) == 2 && card_value(current_player_cards[1]) == card_value(current_player_cards[2])
        is_ace_pair <- is_pair && current_player_cards[1] == "A"

        if (game_rules$allow_split && is_pair && player_times_split < game_rules$max_resplits) {
          if (is_ace_pair && !game_rules$resplit_aces_allowed && player_times_split > 0) {
            # Cannot resplit aces if already split from aces and resplit_aces_allowed is false
            possible_actions$can_split <- FALSE
          } else {
            possible_actions$can_split <- TRUE
          }
        }
        
        if (game_rules$allow_double && length(current_player_cards) == 2) {
          can_double_this_hand <- FALSE
          if (player_hands[[current_hand_idx]]$original_hand || game_rules$double_after_split) {
             if (game_rules$double_on_any_two_cards) {
                can_double_this_hand <- TRUE
             } else if (player_details$value %in% game_rules$double_on_totals) {
                can_double_this_hand <- TRUE
             }
          }
          possible_actions$can_double <- can_double_this_hand
        }

        action <- strategy_fn(current_player_cards, dealer_up_card, game_rules, possible_actions)

        if (action == "split") {
          if (possible_actions$can_split && length(shoe) >= 2) { # Need at least 2 cards for split
            # Create two new hands from the current one
            hand1_card1 <- current_player_cards[1]
            hand2_card1 <- current_player_cards[2]
            
            # Remove current hand, will be replaced by two new ones
            player_hands[[current_hand_idx]] <- NULL 
            
            # Deal second card to first new hand
            if (length(shoe) < 1) { warning("Shoe ran out splitting hand 1"); break }
            new_card_h1 <- shoe[1]; shoe <- shoe[-1]; count <- count + count_value(new_card_h1)
            hand1 <- list(cards = c(hand1_card1, new_card_h1), bet = current_player_bet, status = "active", times_split = player_times_split + 1, original_hand = FALSE)
            
            # Deal second card to second new hand
            if (length(shoe) < 1) { warning("Shoe ran out splitting hand 2"); break }
            new_card_h2 <- shoe[1]; shoe <- shoe[-1]; count <- count + count_value(new_card_h2)
            hand2 <- list(cards = c(hand2_card1, new_card_h2), bet = current_player_bet, status = "active", times_split = player_times_split + 1, original_hand = FALSE)
            
            # Add new hands to the list to be played (insert them to play next)
            # If current_hand_idx was pointing to the hand we just split, it will now point to the first of the new hands after this.
            # Need to be careful with list modification and loop counter.
            player_hands <- append(player_hands, list(hand1, hand2), after = current_hand_idx -1)
            current_player_cards <- player_hands[[current_hand_idx]]$cards # Update current_player_cards to the first new hand
            
            # Special rule for splitting Aces: usually only one card and done, unless hit_split_aces_allowed
            if (hand1_card1 == "A" && !game_rules$hit_split_aces_allowed) {
              player_hands[[current_hand_idx]]$status <- "stood"
            }
            if (hand2_card1 == "A" && !game_rules$hit_split_aces_allowed) {
              player_hands[[current_hand_idx+1]]$status <- "stood"
            }
            
            # Re-evaluate the first new hand immediately in the next iteration of the inner loop.
            # The outer loop counter (current_hand_idx) will increment later to process the second split hand.
            player_turn_active_for_this_hand <- TRUE # Continue loop for this new hand1
            next # Restart while loop for the new hand1

          } else {
            warning("Strategy chose split when not allowed or shoe empty. Defaulting to hit.")
            action <- "hit"
          }
        }
        
        if (action == "double") {
          if (possible_actions$can_double && length(shoe) >=1) {
            current_player_bet <- current_player_bet * 2 # Update bet for this hand only
            player_hands[[current_hand_idx]]$bet <- current_player_bet
            
            card <- shoe[1]; shoe <- shoe[-1]; count <- count + count_value(card)
            current_player_cards <- c(current_player_cards, card)
            player_hands[[current_hand_idx]]$cards <- current_player_cards
            player_hands[[current_hand_idx]]$status <- "stood" # Doubling means stand after 1 card
            player_turn_active_for_this_hand <- FALSE
          } else {
            warning("Strategy chose double when not allowed or shoe empty. Defaulting to hit.")
            action <- "hit"
          }
        }
        
        if (action == "hit") {
          if (length(shoe) < 1) { warning("Shoe ran out on hit."); player_turn_active_for_this_hand <- FALSE; break }
          card <- shoe[1]; shoe <- shoe[-1]; count <- count + count_value(card)
          current_player_cards <- c(current_player_cards, card)
          player_hands[[current_hand_idx]]$cards <- current_player_cards
          # Loop continues, details re-evaluated at top
        } else if (action == "stand") {
          player_hands[[current_hand_idx]]$status <- "stood"
          player_turn_active_for_this_hand <- FALSE
        } else if (action == "double" || action == "split") {
          # Already handled, or will be handled by loop structure for split
        } else {
          warning(paste("Unknown action:", action, "Defaulting to stand."))
          player_hands[[current_hand_idx]]$status <- "stood"
          player_turn_active_for_this_hand <- FALSE
        }
      } # End of player's turn for this specific hand
      current_hand_idx <- current_hand_idx + 1
    } # End of loop through all player hands (original and split)
  } # End of if/else for initial Player/Dealer Blackjack check

  # Dealer's turn (only if player didn't have initial BJ and dealer didn't have initial BJ)
  dealer_details <- get_hand_details(dealer_hand) # Use dealer_initial_details if available and no cards were played
  if (!(player_initial_details$is_blackjack && !dealer_initial_details$is_blackjack) && !dealer_initial_details$is_blackjack) {
    while (dealer_details$value < 17 || (dealer_details$value == 17 && dealer_details$is_soft && game_rules$dealer_hits_soft_17)) {
      if (length(shoe) < 1) { warning("Shoe ran out during dealer hit"); break }
      card <- shoe[1]; shoe <- shoe[-1]; count <- count + count_value(card)
      dealer_hand <- c(dealer_hand, card)
      dealer_details <- get_hand_details(dealer_hand)
    }
  }
  dealer_final_details <- get_hand_details(dealer_hand)

  # Determine results for each of player's hands if no initial BJs ended game early
  if (!(player_initial_details$is_blackjack || dealer_initial_details$is_blackjack)) {
    for (i in 1:length(player_hands)) {
      hand_info <- player_hands[[i]]
      p_cards <- hand_info$cards
      p_bet <- hand_info$bet
      p_details <- get_hand_details(p_cards)
      
      # Blackjack after split rule
      is_player_effective_blackjack = p_details$is_blackjack && 
                                      (hand_info$original_hand || !game_rules$blackjack_after_split_counts_as_21)

      hand_profit <- 0
      if (is_player_effective_blackjack) {
         # This case should not be hit if original hand was BJ, as that's handled above.
         # This implies a BJ on a split hand. Standard payout, not special BJ payout.
         # Or, if blackjack_after_split_counts_as_21 is FALSE, it is treated as a normal 21.
         # The initial BJ check handles the true BJ payout scenario.
         # So here, we just compare vs dealer. If player has 21 (possibly BJ on split) and dealer not BJ.
         if (dealer_final_details$is_blackjack) { hand_profit <- 0 } # BJ vs BJ is a push
         else { hand_profit <- p_bet * (if(hand_info$original_hand) game_rules$blackjack_payout else 1) } # Original hand gets BJ payout if it occurs here somehow (should be caught earlier)
                                                                                                     # Split hand getting A+10 is 21, normal win unless rules change. Let's simplify to 1x bet for A+10 on split.
         if (!hand_info$original_hand && game_rules$blackjack_after_split_counts_as_21 && p_details$is_blackjack) {
            profit_multiplier = 1 # Just a normal win, not special payout
         } else if (is_player_effective_blackjack && !dealer_final_details$is_blackjack) {
            profit_multiplier = game_rules$blackjack_payout # True BJ
         } else {
            profit_multiplier = 1 # Standard win or part of other logic
         }
         
         if (dealer_final_details$is_blackjack) { hand_profit <- 0}
         else { hand_profit <- p_bet * profit_multiplier}

      } else if (dealer_final_details$is_blackjack) { # Dealer BJ, player no BJ (already handled if player had initial BJ)
        hand_profit <- -p_bet
      } else if (p_details$value > 21) {
        hand_profit <- -p_bet
      } else if (dealer_final_details$value > 21) {
        hand_profit <- p_bet
      } else if (p_details$value > dealer_final_details$value) {
        hand_profit <- p_bet
      } else if (p_details$value < dealer_final_details$value) {
        hand_profit <- -p_bet
      } else { # Push
        hand_profit <- 0
      }
      total_profit <- total_profit + hand_profit
      hands_final_outcomes[[length(hands_final_outcomes)+1]] <- list(hand = p_cards, bet = p_bet, outcome_profit = hand_profit, final_value = p_details$value, status = hand_info$status)
    }
  } else {
    # Profit already determined by initial BJ logic, just ensure the hands_final_outcomes is populated if it wasn't (e.g. dealer BJ)
    if(length(hands_final_outcomes) == 0 && length(player_hands) > 0) {
         hands_final_outcomes[[1]] <- list(hand = player_hands[[1]]$cards, bet = player_hands[[1]]$bet, outcome_profit = total_profit, final_value = get_hand_details(player_hands[[1]]$cards)$value, status="initial_bj_resolution")
    }
  }
  
  return(list(shoe = shoe, count = count, profit = total_profit, hands_played_details = hands_final_outcomes))
}

# Run simulation for multiple hands
simulate_blackjack <- function(n = 10000, strategies, game_rules_input = default_game_rules) {
  all_results <- list()
  
  # Initialize shoe and cut card threshold based on game rules
  shoe <- create_shoe(game_rules_input)
  initial_shoe_size <- game_rules_input$num_decks * 52
  # Reshuffle if cards remaining are less than (1 - penetration_percentage) of total
  # e.g. if penetration is 75%, reshuffle when 25% of cards remain
  cut_card_threshold <- initial_shoe_size * (1 - game_rules_input$penetration_percentage)
  
  count <- 0 # Running count for Hi-Lo

  for (strategy_name in names(strategies)) {
    strategy_fn <- strategies[[strategy_name]]
    
    # Reset shoe and count for each strategy to ensure fair comparison (or not, depending on desired sim type)
    # For this setup, let's give each strategy a fresh shoe sequence for 'n' hands.
    # If we wanted to simulate continuous play across strategies, shoe would not be reset here.
    current_shoe_for_strategy <- create_shoe(game_rules_input) 
    current_count_for_strategy <- 0
    
    bankroll <- 0
    results_strategy <- numeric(n)
    
    for (i in 1:n) {
      # Check for reshuffle based on penetration
      if (length(current_shoe_for_strategy) < cut_card_threshold) {
        current_shoe_for_strategy <- create_shoe(game_rules_input)
        current_count_for_strategy <- 0 # Reset count on shuffle
      }
      
      outcome <- play_hand(current_shoe_for_strategy, current_count_for_strategy, initial_bet = 10, strategy_fn = strategy_fn, game_rules = game_rules_input)
      current_shoe_for_strategy <- outcome$shoe
      current_count_for_strategy <- outcome$count
      results_strategy[i] <- outcome$profit
      bankroll <- bankroll + outcome$profit
    }
    
    all_results[[strategy_name]] <- list(final_bankroll = bankroll, results = results_strategy)
  }
  
  return(all_results)
}

# Define strategies to test
strategies_to_test <- list(
  "BasicHitTo17" = basic_strategy, # Original basic strategy
  "NeverBust12" = never_bust_strategy,
  "MimicDealerRules" = mimic_dealer_strategy 
)

# Run it with default rules
cat("Running simulation with default game rules:\n")
print(default_game_rules)
sim_results_default <- simulate_blackjack(n = 10000, strategies = strategies_to_test, game_rules_input = default_game_rules)

# Summarize for default rules
for (strategy_name in names(sim_results_default)) {
  cat("\n--- Strategy:", strategy_name, "(Default Rules) ---\n")
  strategy_data <- sim_results_default[[strategy_name]]
  cat("Final bankroll:", strategy_data$final_bankroll, "\n")
  cat("EV per hand:", mean(strategy_data$results), "\n")
  cat("Standard deviation:", sd(strategy_data$results), "\n")
  cat("Win rate:", sum(strategy_data$results > 0) / length(strategy_data$results), "\n")
}

# Example: Test with different game rules (e.g., Dealer Stands on Soft 17, 6:5 BJ Payout)
custom_game_rules_H17_6to5 <- default_game_rules
custom_game_rules_H17_6to5$dealer_hits_soft_17 <- FALSE # Dealer stands on all 17s
custom_game_rules_H17_6to5$blackjack_payout <- 1.2     # 6:5 payout
custom_game_rules_H17_6to5$allow_double <- FALSE # Disable doubling for this custom run

cat("\n\nRunning simulation with custom game rules (H17, BJ 6:5, No Doubling):\n")
print(custom_game_rules_H17_6to5)
sim_results_custom <- simulate_blackjack(n = 10000, strategies = strategies_to_test, game_rules_input = custom_game_rules_H17_6to5)

# Summarize for custom rules
for (strategy_name in names(sim_results_custom)) {
  cat("\n--- Strategy:", strategy_name, "(Custom Rules: H17, BJ 6:5, No Doubling) ---\n")
  strategy_data <- sim_results_custom[[strategy_name]]
  cat("Final bankroll:", strategy_data$final_bankroll, "\n")
  cat("EV per hand:", mean(strategy_data$results), "\n")
  cat("Standard deviation:", sd(strategy_data$results), "\n")
  cat("Win rate:", sum(strategy_data$results > 0) / length(strategy_data$results), "\n")
}

# Example: Create a strategy that might split (very naively)
simple_split_strategy <- function(player_hand, dealer_up_card, game_rules, possible_actions) {
  player_details <- get_hand_details(player_hand)
  # Always split pairs if possible (except Aces if not allowed to hit split aces, to avoid forced stand on low value)
  if (possible_actions$can_split) {
    is_ace_pair <- length(player_hand) == 2 && player_hand[1] == "A" && player_hand[2] == "A"
    if (is_ace_pair && !game_rules$hit_split_aces_allowed) {
      # Avoid splitting aces if we can't hit them and might get stuck with low totals
      # This is a simple heuristic; better strategies would evaluate EV.
    } else {
      return("split")
    }
  }
  
  if (possible_actions$can_double && player_details$value %in% c(10,11)) { 
    return("double")
  }
  
  if (player_details$value >= 17) return("stand")
  else return("hit")
}

strategies_to_test_with_split <- list(
  "BasicHitTo17" = basic_strategy,
  "NeverBust12" = never_bust_strategy,
  "MimicDealerRules" = mimic_dealer_strategy,
  "PoorDouble" = poor_double_strategy,
  "SimpleSplit" = simple_split_strategy
)

# Run it with default rules (which now include split options)
cat("Running simulation with default game rules (splitting enabled):\n")
print(default_game_rules)
sim_results_default <- simulate_blackjack(n = 1000, strategies = strategies_to_test_with_split, game_rules_input = default_game_rules) # Reduced n for faster test

# Summarize for default rules
for (strategy_name in names(sim_results_default)) {
  cat("\n--- Strategy:", strategy_name, "(Default Rules) ---\n")
  strategy_data <- sim_results_default[[strategy_name]]
  cat("Final bankroll:", strategy_data$final_bankroll, "\n")
  cat("EV per hand:", mean(strategy_data$results), "\n")
  cat("Standard deviation:", sd(strategy_data$results), "\n")
  cat("Win rate:", sum(strategy_data$results > 0) / length(strategy_data$results), "\n")
  # Deeper analysis example: average number of hands played per round (due to splits)
  # This would require simulate_blackjack to collect more detailed info if desired
}

# Custom rules for testing splitting variations
custom_game_rules_split_aces_no_hit <- default_game_rules
custom_game_rules_split_aces_no_hit$hit_split_aces_allowed <- FALSE
custom_game_rules_split_aces_no_hit$blackjack_after_split_counts_as_21 <- TRUE

cat("\n\nRunning simulation with custom game rules (Split Aces - No Hit, Split BJ is 21):\n")
print(custom_game_rules_split_aces_no_hit)
sim_results_custom_split <- simulate_blackjack(n = 1000, strategies = strategies_to_test_with_split, game_rules_input = custom_game_rules_split_aces_no_hit)

# Summarize for custom split rules
for (strategy_name in names(sim_results_custom_split)) {
  cat("\n--- Strategy:", strategy_name, "(Custom Split Rules) ---\n")
  strategy_data <- sim_results_custom_split[[strategy_name]]
  cat("Final bankroll:", strategy_data$final_bankroll, "\n")
  cat("EV per hand:", mean(strategy_data$results), "\n")
  cat("Standard deviation:", sd(strategy_data$results), "\n")
  cat("Win rate:", sum(strategy_data$results > 0) / length(strategy_data$results), "\n")
}
