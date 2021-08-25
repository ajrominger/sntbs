ntbSimViz(fname = 'inst/first_sim.mp4', J = 6^2, m = 0.5, nu = 0.1, Sm = 20,
          SADm = pika::sad(model = 'stick', par = 0.99),
          nstep = 10, init = NULL,
          pal = function(n) sample(viridis::viridis(n)), interval = 0.5)




ntbSimViz(fname = 'inst/long_sim.mp4', J = 6^2, m = 0.5, nu = 0.1, Sm = 20,
          SADm = pika::sad(model = 'stick', par = 0.99),
          nstep = 500, init = NULL,
          pal = function(n) sample(viridis::viridis(n)), interval = 0.1)


