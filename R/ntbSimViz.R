#' @title Neutral Theory of biodiversity simulation and visualization
#'
#' @description Simulate communities under the neutral theory of biodiversity
#'
#' @param fname file name to save the animation to, ending in .mp4
#' @param J size of the local community (i.e. number of individuals)
#' @param m migration probability
#' @param nu speciation probability
#' @param Sm number of species in the metacommunity
#' @param SADm function defining the metacommunity SAD as a \code{pika::sad}
#' class object
#' @param nstep number of time steps to simulate
#' @param init initial state (optional, defults to NULL)
#'
#' @examples
#' x <- 'foo'
#'
#' @return Saves an animation and silently returns the final state of the
#' community as a list with elements
#'
#' @export


ntbSimViz <- function(fname, J, m, nu, Sm, SADm, nstep, init = NULL) {
    # metacommunity sad
    JJm <- pika::sad2Rank(SADm, Sm)
    JJm <- JJm / sum(JJm)

    # index of current number of local species
    JiMax <- Sm

    # vector to hold local SAD
    if(is.null(init)) {
        # initialize with random draw from metacommunity
        JJ <- rep(0, Sm * 100)
        JJ[1:JiMax] <- rmultinom(1, J, JJm)
    } else {
        # otherwise use `init` and check that it's long enough
        JJ <- init

        if(length(JJ) < Sm) {
            JJ <- c(JJ, rep(0, Sm - length(JJ)))
        }
    }

    # run the simulation and save the animation
    animation::saveVideo(video.name = fname,
              ani.width = 800, ani.height = 800, interval = 0.1, nmax = nstep,
              expr = {
    # pdf(file = fname, width = 5, height = 5)
                  for(i in 1:nstep) {
                      # death
                      dead <- sample(JiMax, 1, prob = JJ[1:JiMax])
                      JJ[dead] <- JJ[dead] - 1


                      if(runif(1) <= nu) {
                          # speciation
                          JJ[JiMax + 1] <- 1
                          JiMax <- JiMax + 1
                      } else if(runif(1) <= m) {
                          # immigration
                          imm <- sample(Sm, 1, prob = JJm)
                          JJ[imm] <- JJ[imm] + 1
                      } else {
                          # local birth
                          birth <- sample(JiMax, 1, prob = JJ[1:JiMax])
                          JJ[birth] <- JJ[birth] + 1
                      }

                      foo <- JJ[JJ > 0]
                      if(length(foo) > 0) {
                          plot(sort(foo, TRUE),
                               ylim = c(0, J), xlim = c(1, Sm * 1))
                      }
                  }

    # dev.off()
    })

}


# test
# ntbSimViz(fname = 'foo.mp4', J = 36, m = 0.5, nu = 0.1, Sm = 20,
#           SADm = pika::sad(model = 'stick', par = 0.99),
#           nstep = 100, init = NULL)


# J <- 10^2
# m <- 0.4
# nu <- 0.1
# B <- 1000
#
# xy <- expand.grid(1:sqrt(J), 1:sqrt(J))
# sp <- rep(0, J)
# Smeta <- 20
# meta <- rfish(Smeta, 0.01)
# spcols <- c('gray', viridis(Smeta), magma(15)[-c(1:6, 15)])
# Sspe <- length(spcols) - Smeta - 1
#
#
# saveVideo(video.name = '2019-05-17_highSchoolEcol/fig_neutral.mp4',
#           ani.width = 800, ani.height = 800, interval = 0.1, nmax = B,
#           expr = {
#               for(i in 1:B) {
#                   # png(paste0('2019-05-17_highSchoolEcol/temp/frame_',
#                   #            indexExpand(i, 1, B, 1), '.png'),
#                   #     width = 5, height = 5, units = 'in', res = 140)
#                   par(mar = rep(0.5, 4), cex = 2)
#                   plot(xy, axes = FALSE, col = spcols[sp + 1], pch = 16, cex = 4)
#                   box()
#                   # dev.off()
#
#                   dead <- sample(length(sp), 1)
#                   yesImm <- runif(1) <= m
#                   yesSpe <- runif(1) <= nu
#                   if(yesSpe) {
#                       sp[dead] <- sample(Smeta + (1:Sspe), 1)
#                   } else if(yesImm) {
#                       sp[dead] <- sample(2:(Smeta + 1), 1, prob = meta)
#                   } else {
#                       sp[dead] <- sample(sp[-dead], 1)
#                   }
#               }
#
#           })
#
#
