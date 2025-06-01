
def log_attempt_number(retry_state):
    """Log the attempt number and the exception that caused the retry."""
    print(
        f"Retrying: "
        f"Attempt number {retry_state.attempt_number} "
        f"failed with {retry_state.outcome.exception()}. "
        f"Waiting {retry_state.next_action.sleep} seconds before next attempt."
    )