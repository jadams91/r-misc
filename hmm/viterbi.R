.argmax <- function(v) {
  return((1:length(v))[v == max(v)][1])
}

viterbi <- function(O, Q, A, B, Pi) {
  .T = length(O)
  N  = length(Q)
  
  trellis = array(dim = c(N, .T))
  rownames(trellis) <- Q
  backpt = array(dim = c(N, .T))
  rownames(backpt) <- Q
  
  # init
  trellis[1:N, 1] <- Pi * B[1:N, O[1]]
  
  # rec
  for (t in (2:.T)) {
    for (q in Q) {
      probs = trellis[1:N, t - 1] * A[1:N, q]
      trellis[q, t] <- max(probs) * B[q, O[t]]
      backpt[q, t] <- Q[.argmax(probs)]
    }
  }
  
  # term
  path = rep(NA, .T)
  s = Q[.argmax(trellis[1:N, .T])]
  for (t in .T:1) {
    path[t] = s
    s = backpt[s, t]
  }
  
  return(path)
}
