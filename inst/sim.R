ntbSimViz(fname = 'first_sim.mp4', J = 6^2, m = 0.5, nu = 0.1, Sm = 20,
          SADm = pika::sad(model = 'stick', par = 0.99),
          nstep = 10, init = NULL,
          pal = function(n) sample(viridis::viridis(n)))




mean(rowSums(matrix(sample(1:6, 2 * 10000, replace = TRUE), ncol = 2)) <= 4)

# 1 and 1
# 1 and 2
# 2 and 1
