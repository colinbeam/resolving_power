#-----
# build_grid
#
# Take an empirical distribution and output a metric grid with AUROC on the x-axis and AUPR on the y-axis
#
## Arguments
#
# baseline_model    empirical distribution of effects from a baseline model
# auroc_distance    auroc distance to travel over the grid
# auroc_steps       number of grid points
# exact_shift       find exact step size for each grid step
# normal_approx     use a normal approximation to the empirical distribution