# Start operatingCharacteristics1() function
###############################################################################
# Brianna Hitt - 12-06-19
# This function is the same as OTC1(), but no longer finds the
#   optimal testing configuration. It only calculates operating
#   characteristics for a specified testing configuration. It
#   allows the user to specify a configuration using the group.sz
#   (initial group size) and pool.szs arguments.
# Brianna Hitt - 02-17-20
# Changed the inputs - removed group.sz and pool.szs
# Replaced with hier.config (a group membership matrix) and rowcol.sz
# Added four-stage hierarchical testing for both non-informative and
#   informative settings

#' @title Calculate operating characteristics for group testing algorithms
#' that use a single-disease assay
#'
#' @description Calculate operating characteristics, such as the expected
#' number of tests, for a specified testing configuration using
#' non-informative and informative hierarchical and array-based group testing
#' algorithms. Single-disease assays are used at each stage of the algorithms.
#'
#' @param algorithm character string defining the group testing algorithm to be
#' used. Non-informative testing options include two-stage hierarchical
#' ("\kbd{D2}"), three-stage hierarchical ("\kbd{D3}"), four-stage hierarchical
#' ("\kbd{D4}"), square array testing without master pooling ("\kbd{A2}"), and
#' square array testing with master pooling ("\kbd{A2M}"). Informative testing
#' options include two-stage hierarchical ("\kbd{ID2}"), three-stage
#' hierarchical ("\kbd{ID3}"), four-stage hierarchical ("\kbd{ID4}"), and
#' square array testing without master pooling ("\kbd{IA2}").
#' @param p overall probability of disease that will be used to generate a
#' vector/matrix of individual probabilities. For non-informative algorithms,
#' a homogeneous set of probabilities will be used. For informative algorithms,
#' the \code{\link{expectOrderBeta}} function will be used to generate a
#' heterogeneous set of probabilities. Further details are given under
#' 'Details'. Either \kbd{p} or \kbd{probabilities} should be specified, but
#' not both.
#' @param probabilities a vector of individual probabilities, which is
#' homogeneous for non-informative testing algorithms and heterogeneous for
#' informative testing algorithms. Either  \kbd{p} or \kbd{probabilities}
#' should be specified, but not both.
#' @param Se a vector of sensitivity values, where one value is given for each
#' stage of testing (in order). If a single value is provided, sensitivity
#' values are assumed to be equal to this value for all stages of testing.
#' Further details are given under 'Details'.
#' @param Sp a vector of specificity values, where one value is given for each
#' stage of testing (in order). If a single value is provided, specificity
#' values are assumed to be equal to this value for all stages of testing.
#' Further details are given under 'Details'.
#' @param hier.config a matrix specifying the configuration for a hierarchical
#' testing algorithm. The rows correspond to the stages of testing, the columns
#' correspond to each individual to be tested, and the cell values
#' specify the group number of each individual at each stage. Further details
#' are given under 'Details'. For array testing algorithms, this argument will
#' be ignored.
#' @param rowcol.sz the row/column size for array testing algorithms. For
#' hierarchical testing algorithms, this argument will be ignored.
#' @param alpha a shape parameter for the beta distribution that specifies the
#' degree of heterogeneity for the generated probability vector (for
#' informative testing only).
#' @param a a vector containing indices indicating which individuals to
#' calculate individual accuracy measures for. If \kbd{NULL}, individual
#' accuracy measures will be displayed for all individuals in the algorithm.
#' @param print.time a logical value indicating whether the length of time
#' for calculations should be printed. The default is \kbd{TRUE}.
#' @param ... arguments to be passed to the \code{\link{expectOrderBeta}}
#' function, which generates a vector of probabilities for informative testing
#' algorithms. Further details are given under 'Details'.
#'
#' @details This function computes the operating characteristics for
#' group testing algorithms with an assay that tests for one disease, as
#' described in Hitt et al. (2019).
#'
#' Available algorithms include two-, three-, and four-stage hierarchical
#' testing and array testing with and without master pooling. Both
#' non-informative and informative group testing settings are allowed for each
#' algorithm, except informative array testing with master pooling is
#' unavailable because this method has not appeared in the group testing
#' literature. Operating characteristics calculated are expected number of
#' tests, pooling sensitivity, pooling specificity, pooling positive predictive
#' value, and pooling negative predictive value for each individual.
#'
#' For informative algorithms where the \kbd{p} argument is specified, the
#' expected value of order statistics from a beta distribution are found.
#' These values are used to represent disease risk probabilities for each
#' individual to be tested. The beta distribution has two parameters: a mean
#' parameter \kbd{p} (overall disease prevalence) and a shape parameter
#' \kbd{alpha} (heterogeneity level). Depending on the specified \kbd{p},
#' \kbd{alpha}, and overall group size, simulation may be necessary to
#' generate the vector of individual probabilities. This is done using
#' \code{\link{expectOrderBeta}} and requires the user to set a seed to
#' reproduce results.
#'
#' The sensitivity/specificity values are allowed to vary across stages of
#' testing. For hierarchical testing, a different sensitivity/specificity value
#' may be used for each stage of testing. For array testing, a different
#' sensitivity/specificity value may be used for master pool testing (if
#' included), row/column testing, and individual testing. The values must be
#' specified in order of the testing performed. For example, values are
#' specified as (stage 1, stage 2, stage 3) for three-stage hierarchical
#' testing or (master pool testing, row/column testing, individual testing) for
#' array testing with master pooling. A single sensitivity/specificity value
#' may be specified instead. In this situation, sensitivity/specificity values
#' for all stages are assumed to be equal.
#'
#' The matrix specified  by \kbd{hier.config} defines the hierarchical group
#' testing algorithm for \eqn{I} individuals. The rows of the matrix
#' correspond to the stages \eqn{s=1,...,S} in the testing algorithm, and the
#' columns correspond to individuals \eqn{i=1,...I}. The cell values within
#' the matrix represent the group number of individual \eqn{i} at stage
#' \eqn{s}. For three-stage, four-stage, and non-informative two-stage
#' hierarchical testing, the first row of the matrix consists of all ones.
#' This indicates that all individuals in the algorithm are tested together in
#' a single group in the first stage of testing. For informative two-stage
#' hierarchical testing, the initial group (block) is not tested. Thus, the
#' first row of the matrix consists of the group numbers for each individual
#' in the first stage of testing. For all hierarchical algorithms, the final
#' row of the matrix denotes individual testing. Individuals who are not tested
#' in a particular stage are represented by "NA" (e.g., an individual tested
#' in a group of size 1 in the second stage of testing would not be tested
#' again in a third stage of testing). It is important to note that this
#' matrix represents the testing that could be performed if each group tests
#' positively at each stage prior to the last. For more details on this matrix
#' (called a group membership matrix), see Bilder et al. (2019).
#'
#' For array testing without master pooling, the \kbd{rowcol.sz} specified
#' represents the row/column size for initial (stage 1) testing. For array
#' testing with master pooling, the \kbd{rowcol.sz} specified represents the
#' row/column size for stage 2 testing. This is because the master pool size
#' is the overall array size, given by the square of the row/column size.
#'
#' The displayed overall pooling sensitivity, pooling specificity, pooling
#' positive predictive value, and pooling negative predictive value are
#' weighted averages of the corresponding individual accuracy measures for all
#' individuals within the initial group (or block) for a hierarchical
#' algorithm, or within the entire array for an array-based algorithm.
#' Expressions for these averages are provided in the Supplementary
#' Material for Hitt et al. (2019). These expressions are based on accuracy
#' definitions given by Altman and Bland (1994a, 1994b).
#'
#' The \kbd{operatingCharacteristics1} function accepts additional arguments,
#' namely \kbd{num.sim}, to be passed to the \code{\link{expectOrderBeta}}
#' function, which generates a vector of probabilities for informative group
#' testing algorithms. The \kbd{num.sim} argument specifies the number of
#' simulations from the beta distribution when simulation is used. By default,
#' 10,000 simulations are used.
#'
#' @return A list containing:
#' \item{algorithm}{the group testing algorithm used for calculations.}
#' \item{prob}{the probability of disease or the vector of individual
#' probabilities, as specified by the user.}
#' \item{alpha}{level of heterogeneity for the generated probability vector
#' (for informative testing only).}
#' \item{Se}{the vector of sensitivity values for each stage of testing.}
#' \item{Sp}{the vector of specificity values for each stage of testing.}
#' \item{Config}{a list specifying elements of the specified testing
#' configuration, which may include:
#' \describe{
#' \item{Stage1}{group size for the first stage of hierarchical testing, if
#' applicable.}
#' \item{Stage2}{group sizes for the second stage of hierarchical testing, if
#' applicable.}
#' \item{Stage3}{group sizes for the third stage of hierarchical testing, if
#' applicable.}
#' \item{Block.sz}{the block size/initial group size for informative Dorfman
#' testing, which is not tested.}
#' \item{pool.szs}{group sizes for the first stage of testing for informative
#' Dorfman testing.}
#' \item{Array.dim}{the row/column size for array testing.}
#' \item{Array.sz}{the overall array size for array testing (the square of the
#' row/column size).}}}
#' \item{p.vec}{the sorted vector of individual probabilities, if applicable.}
#' \item{p.mat}{the sorted matrix of individual probabilities in gradient
#' arrangement, if applicable. Further details are given under 'Details'.}
#' \item{ET}{the expected testing expenditure to decode all individuals in the
#' algorithm; this includes all individuals in all groups for hierarchical
#' algorithms or in the entire array for array testing.}
#' \item{value}{the value of the expected number of tests per individual.}
#' \item{Accuracy}{a list containing:
#' \describe{
#' \item{Individual}{a matrix of accuracy measures for each individual
#' specified in \kbd{a}. The rows correspond to each unique set of accuracy
#' measures in the algorithm. Individuals with the same set of accuracy
#' measures are displayed together in a single row of the matrix. The columns
#' correspond to the pooling sensitivity, pooling specificity, pooling
#' positive predictive value, pooling negative predictive value, and the
#' indices for the individuals in each row of the matrix.}
#' \item{Overall}{a matrix of overall accuracy measures for the algorithm.
#' The columns correspond to the pooling sensitivity, pooling specificity,
#' pooling positive predictive value, and pooling negative predictive value
#' for the overall algorithm. Further details are given under 'Details'.}}}
#'
#' @section Note: This function returns the pooling positive and negative
#' predictive values for all individuals even though these measures are
#' diagnostic specific; e.g., the pooling positive predictive value should
#' only be considered for those individuals who have tested positive.
#'
#' Additionally, only stage dependent sensitivity and specificity values are
#' allowed within the program (no group within stage dependent values are
#' allowed). See Bilder et al. (2019) for additional information.
#'
#' @author Brianna D. Hitt
#'
#' @references
#' \insertRef{Altman1994a}{binGroup2}
#'
#' \insertRef{Altman1994b}{binGroup2}
#'
#' \insertRef{Bilder2019}{binGroup2}
#'
#' \insertRef{Hitt2019}{binGroup2}
#'
#' \insertRef{McMahan2012a}{binGroup2}
#'
#' \insertRef{McMahan2012b}{binGroup2}
#'
#' @family operating characteristic functions
#'
#' @examples
#' # Calculate the operating characteristics for non-informative
#' #   two-stage hierarchical (Dorfman) testing.
#' config.mat <- matrix(data = c(rep(1, 10), 1:10),
#'                      nrow = 2, ncol = 10, byrow = TRUE)
#' opChar1(algorithm = "D2", p = 0.05, Se = 0.99, Sp = 0.99,
#'         hier.config = config.mat, print.time = FALSE)
#'
#' # Calculate the operating characteristics for informative
#' #   two-stage hierarchical (Dorfman) testing.
#' # A vector of individual probabilities is generated using
#' #   the expected value of order statistics from a beta
#' #   distribution with p = 0.01 and a heterogeneity level
#' #   of alpha = 0.5.
#' config.mat <- matrix(data = c(rep(1:3, each = 10), 1:30),
#'                      nrow = 2, ncol = 30, byrow = TRUE)
#' set.seed(52613)
#' opChar1(algorithm = "ID2", p = 0.01, Se = 0.95, Sp = 0.95,
#'         hier.config = config.mat, alpha = 0.5, num.sim = 10000)
#' # Equivalent code using a heterogeneous vector of
#' #   probabilities
#' set.seed(52613)
#' probs <- expectOrderBeta(p = 0.01, alpha = 0.5, size = 30)
#' opChar1(algorithm = "ID2", probabilities = probs,
#'         Se = 0.95, Sp = 0.95, hier.config = config.mat)
#'
#' # Calculate the operating characteristics for
#' #   non-informative three-stage hierarchical testing.
#' config.mat <- matrix(data = c(rep(1, 18), rep(1:3, each = 5),
#'                               rep(4, 3), 1:18),
#'                     nrow = 3, ncol = 18, byrow = TRUE)
#' opChar1(algorithm = "D3", p = 0.001, Se = 0.95, Sp = 0.95,
#'         hier.config = config.mat)
#' opChar1(algorithm = "D3", p = 0.001, Se = c(0.95, 0.95, 0.99),
#'         Sp = c(0.96, 0.96, 0.98), hier.config = config.mat)
#'
#' # Calculate the operating characteristics for
#' #   informative three-stage hierarchical testing,
#' #   given a heterogeneous vector of probabilities.
#' config.mat <- matrix(data = c(rep(1, 6), rep(1:2, each = 3),
#'                               1:6), nrow = 3, ncol = 6,
#'                      byrow = TRUE)
#' set.seed(52613)
#' opChar1(algorithm = "ID3",
#'          probabilities = c(0.012, 0.014, 0.011, 0.012, 0.010, 0.015),
#'          Se = 0.99, Sp = 0.99, hier.config = config.mat,
#'          alpha = 0.5, num.sim = 5000)
#'
#' # Calculate the operating characteristics for
#' #   non-informative four-stage hierarchical testing.
#' config.mat <- matrix(data = c(rep(1, 12), rep(1, 8),
#'                               rep(2, 2), 3, 4, rep(1, 5),
#'                               rep(2, 3), 3, 4, rep(NA, 2),
#'                               1:8, rep(NA, 4)), nrow = 4,
#'                      ncol = 12, byrow = TRUE)
#' opChar1(algorithm = "D4", p = 0.041, Se = 0.99, Sp = 0.90,
#'         hier.config = config.mat)
#'
#' # Calculate the operating characteristics for
#' #   informative four-stage hierarchical testing.
#' # A vector of individual probabilities is generated using
#' #   the expected value of order statistics from a beta
#' #   distribution with p = 0.041 and a heterogeneity level
#' #   of alpha = 0.5.
#' config.mat <- matrix(data = c(rep(1, 12), rep(1, 8),
#'                               rep(2, 2), 3, 4, rep(1, 5),
#'                               rep(2, 3), 3, 4, rep(NA, 2),
#'                               1:8, rep(NA, 4)), nrow = 4,
#'                      ncol = 12, byrow = TRUE)
#' set.seed(5678)
#' opChar1(algorithm = "ID4", p = 0.041, Se = 0.99, Sp = 0.90,
#'         hier.config = config.mat, alpha = 0.5)
#'
#' # Calculate the operating characteristics for
#' #   non-informative array testing without master pooling.
#' opChar1(algorithm = "A2", p = 0.005, Se = c(0.95, 0.99),
#'         Sp = c(0.95, 0.99), rowcol.sz = 8, a = 1)
#'
#' # Calculate the operating characteristics for
#' #   informative array testing without master pooling.
#' # A vector of individual probabilities is generated using
#' #   the expected value of order statistics from a beta
#' #   distribution with p = 0.03 and a heterogeneity level
#' #   of alpha = 2.
#' set.seed(1002)
#' opChar1(algorithm = "IA2", p = 0.03, Se = 0.95, Sp = 0.95,
#'          rowcol.sz = 8, alpha = 2, a = 1:10)
#'
#' # Calculate the operating characteristics for
#' #   non-informative array testing with master pooling.
#' opChar1(algorithm = "A2M", p = 0.02, Se = c(0.95,0.95,0.99),
#'         Sp = c(0.98,0.98,0.99), rowcol.sz = 5)

# Brianna Hitt - 04.02.2020
# Changed cat() to warning()

# Brianna Hitt - 06.08.2020
# Allowed for group sizes of 2

# Brianna Hitt - 03.11.2021
# Removed "MAR" as a default objective function
# Changed warning() to message() for large initial group sizes that will
#   take significant time to run in OTC1(); message() allows the warning text
#   to display at the function start rather than at its completion. The warning
#   text was also edited.

# Brianna Hitt - 10.18.2023
# Added function security to ensure probabilities, Se, and Sp
#   are all between 0 and 1

operatingCharacteristics1 <- function(algorithm, p = NULL,
                                      probabilities = NULL,
                                      Se = 0.99, Sp = 0.99,
                                      hier.config = NULL,
                                      rowcol.sz = NULL, alpha = 2,
                                      a = NULL, print.time = TRUE, ...) {

  ## make sure that all necessary information is included in the correct format
  if (!(algorithm %in% c("D2", "D3", "D4", "A2", "A2M",
                         "ID2", "ID3", "ID4", "IA2"))) {
    stop("Please specify one of the following algorithms: D2, ID2, D3, ID3, D4, ID4, A2, IA2, A2M.")
  }

  if (algorithm %in% c("D2", "D3", "D4", "ID2", "ID3", "ID4")) {
    if (is.null(hier.config) |
        (algorithm %in% c("D2", "ID2") & nrow(hier.config) != 2) |
        (algorithm %in% c("D3", "ID3") & nrow(hier.config) != 3) |
        (algorithm %in% c("D4", "ID4") & nrow(hier.config) != 4)) {
      stop("Please provide a matrix specifying the configuration for the hierarchical algorithm. The rows correspond to the stages of testing, the columns correspond to each individual in the algorithm, and the cell values specify the group number of each individual at each stage.\n")
    } else {
      group.sz <- ncol(hier.config)
      dimnames(hier.config) <- NULL
    }
  } else if (algorithm %in% c("A2", "IA2", "A2M")) {
    if (is.null(rowcol.sz)) {
      stop("Please provide a row/column size for the array testing algorithm.\n")
    } else {
      group.sz <- rowcol.sz
    }
  }
  # if ((algorithm %in% c("D2", "D3", "D4", "ID2", "ID3", "ID4") &
  #      !is.null(rowcol.sz)) |
  #     (algorithm %in% c("A2", "IA2", "A2M") & !is.null(hier.config))) {
  #   stop("Please specify a configuration matrix for a hierarchical testing algorithm, or specify a row/column size for an array testing algorithm.\n")
  # }

  # the code below is for when hier.config is a list
  # if (algorithm %in% c("D2", "D3", "D4", "ID2", "ID3", "ID4")) {
  #   if (is.null(hier.config) |
  #       (algorithm %in% c("D2", "ID2") & length(hier.config) != 1) |
  #       (algorithm %in% c("D3", "ID3") & length(hier.config) != 2) |
  #       (algorithm %in% c("D4", "ID4") & length(hier.config) != 3)) {
  #     stop("Please provide a list specifying the configuration for the hierarchical algorithm. Each item in the list corresponds to the group sizes for each stage of testing. The list should not include an item detailing the individual stage of testing.\n")
  #   } else {
  #     group.sz <- hier.config[1]
  #   }
  # } else if (algorithm %in% c("A2", "IA2", "A2M")) {
  #   if (is.null(rowcol.sz)) {
  #     stop("Please provide a row/column size for the array testing algorithm.\n")
  #   } else {
  #     group.sz <- sum(hier.config[1])
  #   }
  # }

  if (algorithm %in% c("D3", "ID3")) {
    stage2 <- get.pools(hier.config[2,])
    stage3 <- NULL
  } else if (algorithm %in% c("D4", "ID4")) {
    stage2 <- get.pools(hier.config[2,])
    stage3 <- get.pools(hier.config[3,])
  } else if (algorithm == "ID2") {
    stage2 <- get.pools(hier.config[1,])
    stage3 <- NULL
  } else if (algorithm %in% c("A2", "IA2", "A2M")) {
    stage2 <- NULL
    stage3 <- NULL
  }
  # if (algorithm %in% c("D3", "ID3")) {
  #   stage2 <- hier.config[2]
  #   stage3 <- NULL
  # } else if (algorithm %in% c("D4", "ID4")) {
  #   stage2 <- hier.config[2]
  #   stage3 <- hier.config[3]
  # } else if (algorithm == "ID2") {
  #   stage2 <- hier.config[1]
  #   stage3 <- NULL
  # }

  if (is.null(p) & is.null(probabilities)) {
    stop("Please specify an overall probability of disease using the 'p' argument, or specify a vector of individual probabilities using the 'probabilities' argument.\n")
  } else if (!is.null(p) & !is.null(probabilities)) {
    stop("You have specified both an overall probability of disease AND a vector of individual probabilities. Please specify only one option.\n")
  } else {
    if (!is.null(p)) {
      if (length(p) > 1) {
        stop("You have specified a probability vector instead of an overall probability of disease. Please specify an overall probability of disease, and the probability vector will be generated based on the algorithm specified for each group size included in the range.\n")
      }

      if (p > 1 | p < 0) {
        stop("Please specify an overall probability of disease between 0 and 1.\n")
      }
    }

    if (!is.null(probabilities)) {
      if (any(probabilities > 1) | any(probabilities < 0)) {
        stop("Please specify individual probabilities between 0 and 1.\n")
      }
      if ((algorithm %in% c("D2", "D3", "D4", "ID2", "ID3", "ID4")) &
          length(probabilities) != group.sz) {
        stop("The vector of individual probabilities is not the correct length. Please make sure that the length of the probability vector is the same as the specified group size.\n")
      } else if ((algorithm %in% c("A2", "A2M", "IA2")) &
                 length(probabilities) != group.sz^2) {
        stop("The vector of individual probabilities is not the correct length. Please make sure that the length of the probability vector is the same as the overall array size (the square of the specified group size).\n")
      }
      if ((algorithm %in% c("D2", "D3", "D4", "A2", "A2M")) &
          all.equal(probabilities, rep(probabilities[1],
                                       length(probabilities))) != TRUE) {
        stop("You have specified a heterogeneous probability vector for a non-informative algorithm. Please specify a homogeneous probability vector using the 'probabilities' argument or specify an overall probability of disease using the 'p' argument.\n")
      }
    }
  }

  if (length(Se) == 1) {
    if (Se < 0 | Se > 1) {
      stop("Please provide sensitivity values between 0 and 1.\n")
    }
  }
  else if (length(Se) > 1) {
    if (any(Se < 0) | any(Se > 1)) {
      stop("Please provide sensitivity values between 0 and 1.\n")
    }

    if ((algorithm %in% c("D2", "ID2", "A2", "IA2") & length(Se) != 2) |
        (algorithm %in% c("D3", "ID3", "A2M") & length(Se) != 3) |
        (algorithm %in% c("D4", "ID4") & length(Se) != 4)) {
      stop("The vector of sensitivity values is not the correct length. Please specify a vector of sensitivity values (one for each stage of testing, in order), or a single value for all stages of testing.\n")
    }
  }

  if (length(Sp) == 1) {
    if (Sp < 0 | Sp > 1) {
      stop("Please provide specificity values between 0 and 1.\n")
    }
  }
  else if (length(Sp) > 1) {
    if (any(Sp < 0) | any(Sp > 1)) {
      stop("Please provide specificity values between 0 and 1.\n")
    }

    if ((algorithm %in% c("D2", "ID2", "A2", "IA2") & length(Sp) != 2) |
        (algorithm %in% c("D3", "ID3", "A2M") & length(Sp) != 3) |
        (algorithm %in% c("D4", "ID4") & length(Sp) != 4)) {
      stop("The vector of specificity values is not the correct length. Please specify a vector of specificity values (one for each stage of testing, in order), or a single value for all stages of testing.\n")
    }
  }

  Se <- generate.acc(algorithm = algorithm, diseases = 1, value = Se,
                     label = "sens")
  Sp <- generate.acc(algorithm = algorithm, diseases = 1, value = Sp,
                     label = "spec")

  # check for an initial group when using hierarchical testing
  if (algorithm %in% c("D2", "D3", "D4", "ID3", "ID4")) {
    if (all.equal(hier.config[1,], rep(1, ncol(hier.config))) != TRUE) {
      stop("Please specify a configuration where all individuals in the algorithm are tested together in the first stage of testing.\n")
    }
  } else if (algorithm == "ID2") {
    if (all.equal(hier.config[1,], rep(1, ncol(hier.config))) == TRUE) {
      stop("Please specify a configuration for informative two-stage hierarchical (PSOD) testing.\n")
    }
  }
  # the code below is for when hier.config is a list
  # if (algorithm == "D2" & length(hier.config[1]) > 1) {
  #   stop("Please provide a list specifying the configuration of the hierarchical algorithm. For non-informative two-stage hierarchical testing, the list should consist of a single component specifying the initial group size.\n")
  # }
  # if (algorithm %in% c("D3", "ID3") & length(hier.config[1]) > 1) {
  #   stop("Please provide a list specifying the configuration of the hierarchical algorithm. For three-stage hierarchical testing, the list should consist of two components. The first component specifies the initial group size and the second component specifies the group sizes for the second stage of testing.\n")
  # }
  # if (algorithm %in% c("D4", "ID4") & length(hier.config[1]) > 1) {
  #   stop("Please provide a list specifying the configuration of the hierarchical algorithm. For four-stage hierarchical testing, the list should consist of three components. The first component specifies the initial group size. The second and third components specify the group sizes for the second and third stages of testing, respectively.\n")
  # }
  # if (algorithm == "ID2" & length(hier.config[1]) <= 1) {
  #   stop("Please provide a list specifying the configuration of the hierarchical algorithm. For informative two-stage hierarchical testing, the list should consist of a single component specifying the group sizes for the first stage of testing.\n")
  # }

  # check the minimum and maximum group sizes
  if (group.sz < 3) {
    if (algorithm %in% c("D3", "D4", "ID2", "ID3", "ID4")) {
      stop("Please specify a configuration with an initial group size of at least 3.\n")
    }
    if (group.sz < 2) {
      if (algorithm %in% c("D2")) {
        stop("Please specify a configuration with an initial group size of at least 2.\n")
      } else if (algorithm %in% c("A2", "IA2", "A2M")) {
        stop("Please specify a row/column size of at least 2.\n")
      }
    }
  }
  # the code below is for when hier.config is a list
  # if ((algorithm %in% c("D2", "D3", "D4", "ID3", "ID4") &
  #      hier.config[1] < 3) |
  #     (algorithm == "ID2" & sum(hier.config[1]) < 3)) {
  #   stop("Please specify a configuration with an initial group size of at least 3.\n")
  # }

  if (group.sz >= 50) {
    if (algorithm %in% c("D3", "D4", "ID2", "ID3", "ID4")) {
      message("Note: Because the maximum group size is 50 or larger, this function may take a significant amount of time to run. Press 'ESC' if you wish to cancel the submitted statements.\n")
    } else if (algorithm %in% c("A2", "IA2", "A2M")) {
      message("Note: Because the maximum row/column size is 50 or larger, this function may take a significant amount of time to run. Press 'ESC' if you wish to cancel the submitted statements.\n")
    }
  }
  # the code below is for when hier.config is a list
  # if ((algorithm %in% c("D3", "D4", "ID3", "ID4") & hier.config[1] >= 50) |
  #     (algorithm == "ID2" & sum(hier.config[1]) >= 50)) {
  #   warning("You have specified a configuration with a group size of 50 or larger. This function may take a VERY long time to run. Press 'ESC' if you wish to cancel the submitted statements.\n")
  # }

  if (!is.null(a)) {
    if (algorithm %in% c("D2", "D3", "D4", "ID2", "ID3", "ID4")) {
      if (length(a) == 1) {
        if (a > group.sz) {
          stop("Please specify values for 'a' between 1 and the number of individuals being tested.\n")
        }
      } else if (length(a) > 1) {
        if (any(a > group.sz)) {
          stop("Please specify values for 'a' between 1 and the number of individuals being tested.\n")
        }
      }
    } else if (algorithm %in% c("A2", "IA2", "A2M")) {
      if (length(a) == 1) {
        if (a > group.sz^2) {
          stop("Please specify values for 'a' between 1 and the number of individuals being tested.\n")
        }
      } else if (length(a) > 1) {
        if (any(a > group.sz^2)) {
          stop("Please specify values for 'a' between 1 and the number of individuals being tested.\n")
        }
      }
    }

  } else {
    if (algorithm %in% c("D2", "D3", "D4", "ID2", "ID3", "ID4")) {
      a <- 1:group.sz
    } else if (algorithm %in% c("A2", "A2M", "IA2")) {
      a <- 1:group.sz^2
    }
  }

  # call function for non-informative two-stage hierarchical (Dorfman) testing
  if (algorithm == "D2") {
    if (!is.null(p)) {
      results <- NI.Dorf.calc1(p = p, Se = Se, Sp = Sp,
                               group.sz = group.sz, a = a,
                               trace = trace, print.time = print.time)
    } else if (!is.null(probabilities)) {
      results <- NI.Dorf.calc1(p = probabilities, Se = Se, Sp = Sp,
                               group.sz = group.sz, a = a,
                               trace = trace, print.time = print.time)
    }
  }

  # call function for non-informative three-stage hierarchical testing
  if (algorithm == "D3") {
    if (!is.null(p)) {
      results <- NI.D3.calc1(p = p, Se = Se, Sp = Sp,
                             group.sz = group.sz, pool.szs = stage2, a = a,
                             trace = trace, print.time = print.time)
    } else if (!is.null(probabilities)) {
      results <- NI.D3.calc1(p = probabilities, Se = Se, Sp = Sp,
                             group.sz = group.sz, pool.szs = stage2, a = a,
                             trace = trace, print.time = print.time)
    }
  }

  # call function for non-informative four-stage hierarchical testing
  if (algorithm == "D4") {
    if (!is.null(p)) {
      results <- NI.D4.calc1(p = p, Se = Se, Sp = Sp,
                             group.sz = group.sz, stage2 = stage2,
                             stage3 = stage3, a = a, trace =
                               trace, print.time = print.time)
    } else if (!is.null(probabilities)) {
      results <- NI.D4.calc1(p = probabilities, Se = Se, Sp = Sp,
                             group.sz = group.sz, stage2 = stage2,
                             stage3 = stage3, a = a, trace =
                               trace, print.time = print.time)
    }
  }

  # call function for non-informative square array testing without
  # master pooling
  if (algorithm == "A2") {
    if (!is.null(p)) {
      results <- NI.Array.calc1(p = p, Se = Se, Sp = Sp,
                                group.sz = group.sz, a = a,
                                trace = trace, print.time = print.time)
    } else if (!is.null(probabilities)) {
      results <- NI.Array.calc1(p = probabilities, Se = Se, Sp = Sp,
                                group.sz = group.sz, a = a,
                                trace = trace, print.time = print.time)
    }
  }

  # call function for non-informative square array testing with master pooling
  if (algorithm == "A2M") {
    if (!is.null(p)) {
      results <- NI.A2M.calc1(p = p, Se = Se, Sp = Sp,
                              group.sz = group.sz, a = a,
                              trace = trace, print.time = print.time)
    } else if (!is.null(probabilities)) {
      results <- NI.A2M.calc1(p = probabilities, Se = Se, Sp = Sp,
                              group.sz = group.sz, a = a,
                              trace = trace, print.time = print.time)
    }
  }

  # call function for informative two-stage hierarchical (Dorfman) testing
  if (algorithm == "ID2") {
    if (!is.null(p)) {
      results <- Inf.Dorf.calc1(p = p, Se = Se, Sp = Sp,
                                group.sz = group.sz, pool.szs = stage2,
                                alpha = alpha, a = a, trace =
                                  trace, print.time = print.time, ...)
    } else if (!is.null(probabilities)) {
      results <- Inf.Dorf.calc1(p = probabilities, Se = Se, Sp = Sp,
                                group.sz = group.sz, pool.szs = stage2,
                                alpha = alpha, a = a, trace =
                                  trace, print.time = print.time, ...)
    }
  }

  # call function for informative three-stage hierarchical testing
  if (algorithm == "ID3") {
    if (!is.null(p)) {
      results <- Inf.D3.calc1(p = p, Se = Se, Sp = Sp,
                              group.sz = group.sz, pool.szs = stage2,
                              alpha = alpha, a = a,
                              trace = trace, print.time = print.time, ...)
    } else if (!is.null(probabilities)) {
      results <- Inf.D3.calc1(p = probabilities, Se = Se, Sp = Sp,
                              group.sz = group.sz, pool.szs = stage2,
                              alpha = alpha, a = a,
                              trace = trace, print.time = print.time, ...)
    }
  }

  # call function for informative four-stage hierarchical testing
  if (algorithm == "ID4") {
    if (!is.null(p)) {
      results <- Inf.D4.calc1(p = p, Se = Se, Sp = Sp,
                              group.sz = group.sz, stage2 = stage2,
                              stage3 = stage3, alpha = alpha,
                              a = a, trace = trace,
                              print.time = print.time, ...)
    } else if (!is.null(probabilities)) {
      results <- Inf.D4.calc1(p = probabilities, Se = Se, Sp = Sp,
                              group.sz = group.sz, stage2 = stage2,
                              stage3 = stage3, alpha = alpha,
                              a = a, trace = trace,
                              print.time = print.time, ...)
    }
  }

  # call function for informative square array testing without master pooling
  if (algorithm == "IA2") {
    if (!is.null(p)) {
      results <- Inf.Array.calc1(p = p, Se = Se, Sp = Sp,
                                 group.sz = group.sz, alpha = alpha, a = a,
                                 trace = trace, print.time = print.time, ...)
    } else if (!is.null(probabilities)) {
      results <- Inf.Array.calc1(p = probabilities, Se = Se, Sp = Sp,
                                 group.sz = group.sz, alpha = alpha, a = a,
                                 trace = trace, print.time = print.time, ...)
    }
  }

  class(results) <- "opChar"
  results
}




###############################################################################
#' @rdname operatingCharacteristics1
opChar1 <- operatingCharacteristics1



