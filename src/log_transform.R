log_transform = function(x) {
  # Determine the smoothing constant
  constant = min(x[x > 0])/2.
  # Add the constant to all values in the vector
  x = x + constant
  # Create a relative abundance vector
  x = x/sum(x)
  # Log that value
  log(x)
}