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
#' @param init initial state (optional, defaults to NULL)
#' @param pal function of one argument (number of colors) used to generate a
#' palette (optional, defaults to NULL)
#' @param interval the interval between animation frames
#'
#' @examples
#' x <- 'foo'
#'
#' @return Saves an animation and silently returns the final state of the
#' community as a list with elements
#'
#' @export


ntbSimViz <- function(fname, J, m, nu, Sm, SADm, nstep,
                      init = NULL, pal = NULL, interval = 0.1) {
    # bound on number of new species likely to be added
    n0 <- qbinom(0.95, nstep, nu)
    i <- min(which(1 - pbinom(n0:nstep, nstep, nu) <= .Machine$double.eps^0.5))
    newSpp <- min(i) - 1 + n0


    # colors and symbols for plotting species
    spcols <- pal(Sm + newSpp)
    sppchs <- rep(15:18, each = ceiling((Sm + newSpp) / 4))
    set.seed(2)
    sppchs <- sample(sppchs[1:(Sm + newSpp)])

    # metacommunity sad
    JJm <- pika::sad2Rank(SADm, Sm)
    JJm <- JJm / sum(JJm)

    # index of current number of local species
    JiMax <- Sm

    # visual representation of individuals
    xy <- expand.grid(1:sqrt(J), 1:sqrt(J))

    # vector to hold local SAD
    if(is.null(init)) {
        # initialize with random draw from metacommunity
        JJ <- rep(0, Sm + newSpp)
        JJ[1:JiMax] <- rmultinom(1, J, JJm)
    } else {
        # otherwise use `init` and check that it's long enough
        JJ <- init

        if(length(JJ) < Sm + newSpp) {
            JJ <- c(JJ, rep(0, Sm + newSpp - length(JJ)))
        }
    }

    # run the simulation and save the animation
    animation::saveVideo(video.name = fname,
              ani.width = 1800, ani.height = 1000,
              interval = interval, nmax = nstep,
              expr = {
    # pdf(file = 'foo.pdf', width = 5, height = 5)
                  for(i in 1:nstep) {
                      # death
                      dead <- sample(JiMax, 1, prob = JJ[1:JiMax])
                      JJ[dead] <- JJ[dead] - 1


                      if(runif(1) < nu) {
                          # speciation
                          JJ[JiMax + 1] <- 1
                          JiMax <- JiMax + 1
                      } else if(runif(1) < m) {
                          # immigration
                          imm <- sample(Sm, 1, prob = JJm)
                          JJ[imm] <- JJ[imm] + 1
                      } else {
                          # local birth
                          birth <- sample(JiMax, 1, prob = JJ[1:JiMax])
                          JJ[birth] <- JJ[birth] + 1
                      }

                      # plotting
                      above0 <- JJ > 0
                      plotOrd <- order(JJ[above0], decreasing = TRUE)
                      xyi <- rep(1:length(JJ), JJ)

                      layout(matrix(1:2, nrow = 1), widths = c(2, 1))

                      par(cex = 2, mar = rep(1.5, 4))
                      plot(xy, col = spcols[xyi], pch = sppchs[xyi], cex = 3,
                           axes = FALSE, asp = 1,
                           xlim = c(0, sqrt(J) + 1),
                           ylim = c(0, sqrt(J) + 1))
                      abline(h = (0:sqrt(J)) + 0.5, v = (0:sqrt(J)) + 0.5,
                           col = 'black', lty = 1, lwd = 2)

                      par(cex = 2, mar = c(8, 3, 8, 1) + 0.5, mgp = c(2, 1, 0))
                      plot(JJ[above0][plotOrd], cex = 2,
                           pch = sppchs[above0][plotOrd],
                           col = spcols[above0][plotOrd],
                           xlim = c(1, JiMax), ylim = c(0, max(J / 4, JJ)),
                           xlab = 'Species sorted by abundance',
                           ylab = 'Abundance')
                  }
    })

    try(dev.off(), silent = TRUE)

    invisible(JJ)
}

