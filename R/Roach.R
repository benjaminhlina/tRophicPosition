#' Data frame of Roach in Lough Neagh
#'
#' The [roach](https://fishbase.mnhn.fr/summary/SpeciesSummary.php?ID=272&AT=roach)
#' is a cyprinid freshwater-brackish benthopelagic fish, common to
#' most of Europe and western Asial]
#'. Larvae and juveniles
#'  are typically pelagic, consuming zooplankton, with a switch to more benthic
#'   diets as they grow, including plant material and detritus. The dataset
#'   included here examines if a consumer shows an ontogenetic shift in their
#'   trophic position, studying how TP varies across different size classes.
#'
#' @format A data frame with 6 variables:
#' \describe{
#'   \item{Taxon}{factor, with 5 levels, the common name of each baseline species
#'   and Roach}
#'   \item{FG}{factor, with 3 levels, each representing three functional groups:
#'   Benthic_BL (bith, theodoxus and valvata), Pelagic_BL (zebra mussel) and
#'   Roach (consumer)}
#'   \item{Fork.length}{numeric, fork length of roach in mm}
#'   \item{Size.class}{numeric, each representing deciles of fork length of roach}
#'   \item{d13C}{numeric, stable isotope values of d13C}
#'   \item{d15N}{numeric, stable isotope values of d15N}
#' }
#'
#' @usage data("Roach")
"Roach"
