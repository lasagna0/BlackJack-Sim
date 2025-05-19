set.seed(21)

source("blackjack_hand_logic.r") # Load hand playing logic

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
  allow_surrender = "Late",             # Options: "No" (or FALSE), "Late". Early Surrender not implemented.
  surrender_restrict_on_dealer_ace = FALSE, # If TRUE, surrender not allowed if dealer shows Ace (some casinos)
  allow_insurance = TRUE,             # New: Allow insurance bets
  insurance_true_count_threshold = 3, # Example: Take insurance if TC >= 3
  
  # Betting Strategy Rules
  bet_spread_active = FALSE,          # New: Activate variable betting based on true count
  bet_strategy_fn_name = "flat_bet",# New: Name of the bet strategy function to use ("flat_bet", "hi_lo_bet_variation")
  base_bet_unit = 10,               # New: Base unit for betting calculations
  min_bet = 10,                     # New: Minimum allowed bet
  max_bet = 100,                    # New: Maximum allowed bet (e.g., 10x min bet)
  true_count_bet_levels = list(      # New: Example bet levels for hi_lo_bet_variation
    tc_minus_inf_to_0 = 1, # True Count <= 0: 1 unit
    tc_1 = 1,              # True Count == 1: 1 unit
    tc_2 = 2,              # True Count == 2: 2 units
    tc_3 = 3,              # True Count == 3: 3 units
    tc_4 = 4,              # True Count == 4: 4 units
    tc_5_plus = 5          # True Count >= 5: 5 units
  )
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
basic_strategy <- function(player_hand, dealer_up_card, game_rules, possible_actions, current_true_count) {
  player_details <- get_hand_details(player_hand)
  if (player_details$value >= 17) return("stand")
  else return("hit")
}

# New Strategy: Never Bust (Stand on 12+)
never_bust_strategy <- function(player_hand, dealer_up_card, game_rules, possible_actions, current_true_count) {
  player_details <- get_hand_details(player_hand)
  if (player_details$value >= 12) return("stand")
  else return("hit")
}

# New Strategy: Mimic Dealer
mimic_dealer_strategy <- function(player_hand, dealer_up_card, game_rules, possible_actions, current_true_count) {
  player_details <- get_hand_details(player_hand)
  # Simplistic: doesn't use possible_actions for double/split/surrender yet
  if (player_details$value > 17) return("stand")
  if (player_details$value == 17 && !player_details$is_soft) return("stand")
  if (player_details$value == 17 && player_details$is_soft && !game_rules$dealer_hits_soft_17) return("stand") # Player stands on soft 17 if dealer would
  else return("hit")
}

# Poor Double strategy: very naively doubles, no split/surrender logic yet
poor_double_strategy <- function(player_hand, dealer_up_card, game_rules, possible_actions, current_true_count) {
  player_details <- get_hand_details(player_hand)
  # Simplistic: doesn't use possible_actions for split/surrender yet
  if (isTRUE(possible_actions$can_double) && player_details$value %in% c(10,11)) {
    return("double")
  } else if (player_details$value >= 17) {
    return("stand")
  } else {
    return("hit")
  }
}

# Simple Split strategy: very naively splits/doubles, no surrender logic yet
simple_split_strategy <- function(player_hand, dealer_up_card, game_rules, possible_actions, current_true_count) {
  player_details <- get_hand_details(player_hand)
  # Always split pairs if possible (except Aces if not allowed to hit split aces, to avoid forced stand on low value)
  if (isTRUE(possible_actions$can_split)) {
    is_ace_pair <- length(player_hand) == 2 && player_hand[1] == "A" && player_hand[2] == "A"
    if (is_ace_pair && !game_rules$hit_split_aces_allowed) {
      # Avoid splitting aces if we can't hit them and might get stuck with low totals
      # This is a simple heuristic; better strategies would evaluate EV.
    } else {
      return("split")
    }
  }
  
  if (isTRUE(possible_actions$can_double) && player_details$value %in% c(10,11)) { 
    return("double")
  }
  
  if (player_details$value >= 17) return("stand")
  else return("hit")
}

# Run simulation for multiple hands
simulate_blackjack <- function(n = 10000, strategies, game_rules_input = default_game_rules, output_csv_file = NULL) {
  all_results <- list()
  hand_log_data <- list() # For CSV output
  
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
    current_shoe <- create_shoe(game_rules_input) 
    current_count <- 0
    
    bankroll <- 0
    results_strategy <- numeric(n)
    true_counts_log <- numeric(n)
    bets_log <- numeric(n)
    
    for (i in 1:n) {
      # Check for reshuffle based on penetration
      if (length(current_shoe) < cut_card_threshold) {
        current_shoe <- create_shoe(game_rules_input)
        current_count <- 0 # Reset count on shuffle
      }
      
      # Calculate True Count before betting
      current_true_count <- get_true_count(current_count, length(current_shoe), game_rules_input$num_decks)
      true_counts_log[i] <- current_true_count
      
      # Determine bet amount using betting strategy
      bet_amount <- if(game_rules_input$bet_spread_active) {
                      bet_strategy_functions[[game_rules_input$bet_strategy_fn_name]](bankroll, current_true_count, game_rules_input)
                    } else {
                      game_rules_input$base_bet_unit
                    }
      if (bet_amount == 0 && bankroll <=0) { # Player is tapped out, cannot bet
        # results_strategy[i] <- 0 # No profit/loss if no bet
        # For simulation, maybe stop or log this. For now, assume we can always make min bet if not 0 bankroll.
        # The bet strategy itself handles bankroll check. If it returns 0, means cannot bet.
         if (i > 1) results_strategy[i] <- results_strategy[i-1] else results_strategy[i] <- 0 # Maintain last bankroll if tapped out
         bets_log[i] <- 0
         next # Skip hand if cannot bet
      }
      bets_log[i] <- bet_amount
      
      # Pass current_true_count to play_hand as strategy_fn might use it (e.g. for insurance or other deviations)
      outcome <- play_hand(current_shoe, current_count, initial_bet = bet_amount, strategy_fn = strategy_fn, game_rules = game_rules_input, current_true_count = current_true_count)
      
      current_shoe <- outcome$shoe
      current_count <- outcome$count # play_hand now returns the updated running_count
      results_strategy[i] <- outcome$profit
      bankroll <- bankroll + outcome$profit
      
      # Log hand details if CSV output is enabled
      if (!is.null(output_csv_file)) {
        for (h_detail in outcome$hands_played_details) {
          hand_log_data[[length(hand_log_data) + 1]] <- list(
            simulation_run = strategy_name, 
            hand_number = i,
            true_count_at_bet = current_true_count,
            bet_amount = h_detail$bet, # The bet for that specific hand (could be doubled/split)
            player_hand = paste(h_detail$hand, collapse="-"),
            dealer_initial_hand = paste(outcome$dealer_hand_initial, collapse="-"),
            dealer_final_hand = paste(outcome$dealer_hand_final, collapse="-"),
            profit = h_detail$outcome_profit,
            insurance_profit = outcome$insurance_profit, # Total insurance profit for the round
            status = h_detail$status
          )
        }
         if(length(outcome$hands_played_details) == 0 && !is.null(outcome$profit)) { #e.g. initial BJ resolution
            hand_log_data[[length(hand_log_data) + 1]] <- list(
                simulation_run = strategy_name, hand_number = i, true_count_at_bet = current_true_count,
                bet_amount = bet_amount, player_hand = "N/A_Initial_BJ_Res", 
                dealer_initial_hand = paste(outcome$dealer_hand_initial, collapse="-"),
                dealer_final_hand = paste(outcome$dealer_hand_final, collapse="-"),
                profit = outcome$profit, insurance_profit = outcome$insurance_profit, status = "INITIAL_RESOLUTION"
            )
         }
      }
    }
    
    all_results[[strategy_name]] <- list(final_bankroll = bankroll, results = results_strategy, tc_log = true_counts_log, bet_log = bets_log)
  }
  
  # Write to CSV if filename provided
  if (!is.null(output_csv_file) && length(hand_log_data) > 0) {
    hand_log_df <- do.call(rbind.data.frame, hand_log_data)
    write.csv(hand_log_df, output_csv_file, row.names = FALSE)
    cat("\nDetailed hand log written to:", output_csv_file, "\n")
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
simple_split_strategy <- function(player_hand, dealer_up_card, game_rules, possible_actions, current_true_count) {
  player_details <- get_hand_details(player_hand)
  # Always split pairs if possible (except Aces if not allowed to hit split aces, to avoid forced stand on low value)
  if (isTRUE(possible_actions$can_split)) {
    is_ace_pair <- length(player_hand) == 2 && player_hand[1] == "A" && player_hand[2] == "A"
    if (is_ace_pair && !game_rules$hit_split_aces_allowed) {
      # Avoid splitting aces if we can't hit them and might get stuck with low totals
      # This is a simple heuristic; better strategies would evaluate EV.
    } else {
      return("split")
    }
  }
  
  if (isTRUE(possible_actions$can_double) && player_details$value %in% c(10,11)) { 
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
custom_game_rules_split_aces_no_hit$allow_surrender <- "No" # Disable surrender for this test

cat("\n\nRunning simulation with custom game rules (Split Aces - No Hit, Split BJ is 21, No Surrender):\n")
print(custom_game_rules_split_aces_no_hit)
sim_results_custom_split <- simulate_blackjack(
  n = 1000,
  strategies = strategies_to_test_with_split,
  game_rules_input = custom_game_rules_split_aces_no_hit,
  output_csv_file = "custom_split_results.csv"
)# Summarize for custom split rules
for (strategy_name in names(sim_results_custom_split)) {
  cat("\n--- Strategy:", strategy_name, "(Custom Split Rules) ---\n")
  strategy_data <- sim_results_custom_split[[strategy_name]]
  cat("Final bankroll:", strategy_data$final_bankroll, "\n")
  cat("EV per hand:", mean(strategy_data$results), "\n")
  cat("Standard deviation:", sd(strategy_data$results), "\n")
  cat("Win rate:", sum(strategy_data$results > 0) / length(strategy_data$results), "\n")
}

# Example: Strategy that considers surrender
surrender_strategy <- function(player_hand, dealer_up_card, game_rules, possible_actions, current_true_count) {
  player_details <- get_hand_details(player_hand); dealer_up_value <- card_value(dealer_up_card)
  if (isTRUE(possible_actions$can_surrender)) {
    if (player_details$value == 16 && (dealer_up_value %in% c(9, 10, 11))) return("surrender")
    if (player_details$value == 15 && dealer_up_value == 10) return("surrender")
  }
  if (isTRUE(possible_actions$can_split)) { is_ace_pair <- length(player_hand) == 2 && player_hand[1] == "A" && player_hand[2] == "A"; if (!(is_ace_pair && !game_rules$hit_split_aces_allowed)) return("split") }
  if (isTRUE(possible_actions$can_double) && player_details$value %in% c(10,11)) return("double")
  if (player_details$value >= 17) return("stand") else return("hit")
}

strategies_to_test_all <- list(
  "BasicHitTo17" = basic_strategy,
  "NeverBust12" = never_bust_strategy,
  "MimicDealerRules" = mimic_dealer_strategy,
  "PoorDouble" = poor_double_strategy,
  "SimpleSplit" = simple_split_strategy,
  "SurrenderBasic" = surrender_strategy
)

# Run it with default rules (which now include surrender options)
cat("Running simulation with default game rules (Full Basic Strategy included):\n")
print(default_game_rules)
sim_results_default <- simulate_blackjack(n = 10000, 
                                        strategies = strategies_to_test_all, 
                                        game_rules_input = default_game_rules, 
                                        output_csv_file = "blackjack_simulation_log.csv") # Increased n back

# Summarize for default rules
for (strategy_name in names(sim_results_default)) {
  cat("\n--- Strategy:", strategy_name, "(Default Rules) ---\n")
  strategy_data <- sim_results_default[[strategy_name]]
  cat("Final bankroll:", strategy_data$final_bankroll, "\n")
  cat("EV per hand:", mean(strategy_data$results), "\n")
  cat("Standard deviation:", sd(strategy_data$results), "\n")
  cat("Win rate:", sum(strategy_data$results > 0) / length(strategy_data$results), "\n")
}

# Custom rules for testing 
custom_game_rules_H17_noDAS_noSurrender <- default_game_rules
custom_game_rules_H17_noDAS_noSurrender$dealer_hits_soft_17 <- FALSE
custom_game_rules_H17_noDAS_noSurrender$double_after_split <- FALSE 
custom_game_rules_H17_noDAS_noSurrender$allow_surrender <- "No"
custom_game_rules_H17_noDAS_noSurrender$blackjack_payout <- 1.5 # Keep 3:2

cat("\n\nRunning simulation with custom game rules (H17, no DAS, no Surrender):\n")
print(custom_game_rules_H17_noDAS_noSurrender)
sim_results_custom <- simulate_blackjack(n = 10000, strategies = strategies_to_test_all, game_rules_input = custom_game_rules_H17_noDAS_noSurrender)

# Summarize for custom rules
for (strategy_name in names(sim_results_custom)) {
  cat("\n--- Strategy:", strategy_name, "(Custom Rules: H17, noDAS, noSurr) ---\n")
  strategy_data <- sim_results_custom[[strategy_name]]
  cat("Final bankroll:", strategy_data$final_bankroll, "\n")
  cat("EV per hand:", mean(strategy_data$results), "\n")
  cat("Standard deviation:", sd(strategy_data$results), "\n")
  cat("Win rate:", sum(strategy_data$results > 0) / length(strategy_data$results), "\n")
}

# Full Basic Strategy Implementation
full_basic_strategy <- function(player_hand, dealer_up_card, game_rules, possible_actions, current_true_count) {
  player_details <- get_hand_details(player_hand)
  player_total <- player_details$value
  is_soft_hand <- player_details$is_soft
  
  dealer_up_card_value <- card_value(dealer_up_card)
  # Convert Ace (11) to 1 for dealer card index if common strategy tables use 1 for Ace.
  # Most tables show Ace as 'A' or 11. Let's assume dealer_up_card_value is 2-11.

  # Helper to check if an action is preferred but not possible, and provide fallback
  # For now, this logic will be embedded in the choices below.

  # Rendición (Surrender) - Late Surrender rules assumed
  # Surrender is only an option for the first two cards.
  if (isTRUE(possible_actions$can_surrender) && length(player_hand) == 2) {
    # Common surrender plays (multi-deck, S17):
    # Player 16 surrenders vs dealer 9, 10, Ace
    if (player_total == 16 && !is_soft_hand && dealer_up_card_value %in% c(9, 10, 11)) return("surrender")
    # Player 15 surrenders vs dealer 10
    if (player_total == 15 && !is_soft_hand && dealer_up_card_value == 10) return("surrender")
    # Some strategies also surrender 15 vs Ace, or 17-hard vs Ace if H17 and no surrender on BJ check
    # We will stick to common ones for now.
  }

  # División de Pares (Splitting Pairs)
  if (isTRUE(possible_actions$can_split) && length(player_hand) == 2) {
    card1_value <- card_value(player_hand[1]) # Actual numeric value for splitting logic
    # Aces and 8s are always split
    if (card1_value == 11) return("split") # Split Aces
    if (card1_value == 8) return("split")  # Split 8s
    
    # Other pairs (dependent on dealer upcard and game rules like DAS)
    if (card1_value == 9) {
      if (!(dealer_up_card_value %in% c(7, 10, 11))) return("split") # Split 9s vs 2-6, 8, 9. Stand vs 7, 10, A.
    }
    if (card1_value == 7) {
      if (dealer_up_card_value <= 7) return("split") # Split 7s vs 2-7
    }
    if (card1_value == 6) {
      if (dealer_up_card_value <= 6 && (game_rules$double_after_split || dealer_up_card_value != 2)) { # Split 6s vs 2-6 (or 3-6 if DAS not allowed and dealer has 2)
         # More precise: Split 2-6 if DAS, else 3-6. Assuming DAS for now.
         if (dealer_up_card_value >=2 && dealer_up_card_value <=6) return("split")
      } 
    }
    if (card1_value == 4) {
      # Only split 4s if DAS and dealer shows 5 or 6
      if (game_rules$double_after_split && dealer_up_card_value %in% c(5,6)) return("split")
    }
    if (card1_value %in% c(2,3)) {
      if (dealer_up_card_value <= 7 && (game_rules$double_after_split || dealer_up_card_value < 4 || dealer_up_card_value > 7 )) { 
        # More precise: Split 2s,3s vs 2-7 if DAS, else 4-7. Assuming DAS for now.
        if(dealer_up_card_value >=2 && dealer_up_card_value <=7) return("split")
      }
    }
    # Tens (10, J, Q, K) are not split according to basic strategy.
    # Fives are treated as a hard 10, not split.
  }

  # Manos Suaves (Soft Totals) - Player has an Ace counted as 11
  if (is_soft_hand) {
    if (player_total == 21) return("stand") # Soft 21 (A,10) - already Blackjack if initial 2 cards, or stood if drawn to.
    if (player_total == 20) return("stand") # Soft 20 (A,9) - always stand
    if (player_total == 19) { # Soft 19 (A,8)
      if (dealer_up_card_value == 6 && isTRUE(possible_actions$can_double) && game_rules$double_after_split && length(player_hand) == 2) return("double") # Double vs 6 if allowed (usually DAS only)
      else return("stand")
    }
    if (player_total == 18) { # Soft 18 (A,7)
      if (isTRUE(possible_actions$can_double) && dealer_up_card_value >= 2 && dealer_up_card_value <= 6 && length(player_hand) == 2) return("double")
      else if (dealer_up_card_value >= 9 && dealer_up_card_value <= 11) return("hit") # Hit vs 9, 10, Ace
      else return("stand") # Stand vs 2, 7, 8
    }
    if (player_total == 17) { # Soft 17 (A,6)
      if (isTRUE(possible_actions$can_double) && dealer_up_card_value >= 3 && dealer_up_card_value <= 6 && length(player_hand) == 2) return("double")
      else return("hit")
    }
    if (player_total %in% c(15, 16)) { # Soft 15 (A,4), Soft 16 (A,5)
      if (isTRUE(possible_actions$can_double) && dealer_up_card_value >= 4 && dealer_up_card_value <= 6 && length(player_hand) == 2) return("double")
      else return("hit")
    }
    if (player_total %in% c(13, 14)) { # Soft 13 (A,2), Soft 14 (A,3)
      if (isTRUE(possible_actions$can_double) && dealer_up_card_value >= 5 && dealer_up_card_value <= 6 && length(player_hand) == 2) return("double")
      else return("hit")
    }
    # Soft 12 (A,A) would be handled by pair splitting if A,A is initial. If A is drawn to A, it's soft 12 - this is covered by split logic or subsequent hard/soft logic.
  }

  # Manos Duras (Hard Totals) - No Ace, or Ace counts as 1
  # Player_total is already the hard total if is_soft_hand is FALSE
  if (player_total >= 17) return("stand")
  if (player_total >= 13 && player_total <= 16) { # Player 13-16
    if (dealer_up_card_value >= 2 && dealer_up_card_value <= 6) return("stand")
    else return("hit")
  }
  if (player_total == 12) {
    if (dealer_up_card_value >= 4 && dealer_up_card_value <= 6) return("stand")
    else return("hit")
  }
  if (player_total == 11) {
    if (isTRUE(possible_actions$can_double) && length(player_hand) == 2) return("double") else return("hit")
  }
  if (player_total == 10) {
    if (isTRUE(possible_actions$can_double) && dealer_up_card_value >= 2 && dealer_up_card_value <= 9 && length(player_hand) == 2) return("double") else return("hit")
  }
  if (player_total == 9) {
    if (isTRUE(possible_actions$can_double) && dealer_up_card_value >= 3 && dealer_up_card_value <= 6 && length(player_hand) == 2) return("double") else return("hit")
  }
  if (player_total <= 8) return("hit") 

  # Fallback: if no decision made (should not happen with comprehensive logic)
  warning(paste("FullBasicStrategy: No decision made for hand:", paste(player_hand, collapse=","), "(Value:", player_total, ", Soft:", is_soft_hand, ") vs Dealer:", dealer_up_card, ". Defaulting to stand."))
  return("stand")
}

strategies_to_test_all <- list(
  "BasicHitTo17" = basic_strategy,
  "NeverBust12" = never_bust_strategy,
  "MimicDealerRules" = mimic_dealer_strategy,
  "PoorDouble" = poor_double_strategy,
  "SimpleSplit" = simple_split_strategy,
  "SurrenderBasic" = surrender_strategy,
  "FullBasic" = full_basic_strategy       # Adding the new full basic strategy
)

# Run it with default rules (which now include surrender options)
cat("Running simulation with default game rules (Full Basic Strategy included):\n")
print(default_game_rules)
sim_results_default <- simulate_blackjack(n = 10000, 
                                        strategies = strategies_to_test_all, 
                                        game_rules_input = default_game_rules, 
                                        output_csv_file = "blackjack_simulation_log.csv") # Increased n back

# Summarize for default rules
for (strategy_name in names(sim_results_default)) {
  cat("\n--- Strategy:", strategy_name, "(Default Rules) ---\n")
  strategy_data <- sim_results_default[[strategy_name]]
  cat("Final bankroll:", strategy_data$final_bankroll, "\n")
  cat("EV per hand:", mean(strategy_data$results), "\n")
  cat("Standard deviation:", sd(strategy_data$results), "\n")
  cat("Win rate:", sum(strategy_data$results > 0) / length(strategy_data$results), "\n")
}

# Custom rules for testing 
custom_game_rules_H17_noDAS_noSurrender <- default_game_rules
custom_game_rules_H17_noDAS_noSurrender$dealer_hits_soft_17 <- FALSE
custom_game_rules_H17_noDAS_noSurrender$double_after_split <- FALSE 
custom_game_rules_H17_noDAS_noSurrender$allow_surrender <- "No"
custom_game_rules_H17_noDAS_noSurrender$blackjack_payout <- 1.5 # Keep 3:2

cat("\n\nRunning simulation with custom game rules (H17, no DAS, no Surrender):\n")
print(custom_game_rules_H17_noDAS_noSurrender)
sim_results_custom <- simulate_blackjack(n = 10000, strategies = strategies_to_test_all, game_rules_input = custom_game_rules_H17_noDAS_noSurrender)

# Summarize for custom rules
for (strategy_name in names(sim_results_custom)) {
  cat("\n--- Strategy:", strategy_name, "(Custom Rules: H17, noDAS, noSurr) ---\n")
  strategy_data <- sim_results_custom[[strategy_name]]
  cat("Final bankroll:", strategy_data$final_bankroll, "\n")
  cat("EV per hand:", mean(strategy_data$results), "\n")
  cat("Standard deviation:", sd(strategy_data$results), "\n")
  cat("Win rate:", sum(strategy_data$results > 0) / length(strategy_data$results), "\n")
}
