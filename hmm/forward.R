forward = function(O, Q, A, B, Pi) {
  .T = length(O)
  N  = length(Q)
  
  trellis = array(dim = c(N, .T))
  rownames(trellis) <- Q
  
  # init
  trellis[1:N, 1] <- Pi * B[1:N, O[1]]
  
  # rec
  for (t in (2:.T)) {
    for (q in Q) {
      probs = trellis[1:N, t - 1] * A[1:N, q]
      trellis[q, t] <- sum(probs) * B[q, O[t]]
    }
  }
  
  # term
  return(sum(trellis[1:N, .T]))
}
