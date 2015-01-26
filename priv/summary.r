#!/usr/bin/env Rscript

# Parse the --file= argument out of command line args and
# determine where base directory is so that we can source
# our common sub-routines
arg0 <- sub("--file=(.*)", "\\1", grep("--file=", commandArgs(), value = TRUE))
dir0 <- dirname(arg0)
source(file.path(dir0, "common.r"))

theme_set(theme_grey(base_size = 17))

# Setup parameters for the script
params = matrix(c(
  'help',    'h', 0, "logical",
  'width',   'x', 2, "integer",
  'height',  'y', 2, "integer",
  'outfile', 'o', 2, "character",
  'indir',   'i', 2, "character",
  'tstart',  '1',  2, "integer",
  'tend',    '2',  2, "integer",
  'ylabel1stgraph', 'Y',  2, "character"
  ), ncol=4, byrow=TRUE)

# Parse the parameters
opt = getopt(params)

if (!is.null(opt$help))
  {
    cat(paste(getopt(params, command = basename(arg0), usage = TRUE)))
    q(status=1)
  }

# Initialize defaults for opt
if (is.null(opt$width))   { opt$width   = 1280 }
if (is.null(opt$height))  { opt$height  = 1500 }
if (is.null(opt$indir))   { opt$indir  = "current"}
if (is.null(opt$outfile)) { opt$outfile = file.path(opt$indir, "summary.png") }
if (is.null(opt$ylabel1stgraph)) { opt$ylabel1stgraph = "Op/sec" }

# Load the benchmark data, passing the time-index range we're interested in
b = load_benchmark(opt$indir, opt$tstart, opt$tend)

# If there is no actual data available, bail
if (nrow(b$latencies) == 0)
{
  stop("No latency information available to analyze in ", opt$indir)
}

png(file = opt$outfile, width = opt$width, height = opt$height)

# First plot req/sec from summary
plot_throughput <- qplot(elapsed, successful / window, data = b$summary,
                geom = c("smooth", "point"),
                xlab = "Elapsed Secs", ylab = opt$ylabel1stgraph,
                main = "Throughput") +

                geom_smooth(aes(y = successful / window, colour = "ok"), size=0.5) +
                geom_point(aes(y = successful / window, colour = "ok"), size=2.0) +

                scale_colour_manual("Response", values = c("#188125"))

plot_error <- qplot(elapsed, failed / window, data = b$summary,
                geom = c("smooth", "point"),
                xlab = "Elapsed Secs", ylab = opt$ylabel1stgraph,
                main = "Errors") +

                geom_smooth(aes(y = failed / window, colour = "error"), size=0.5) +
                geom_point(aes(y = failed / window, colour = "error"), size=2.0) +

                scale_colour_manual("Response", values = c("#FF665F"))

plot_sessions_running <- qplot(elapsed, running, data = b$sessions,
                 geom = c("point"),
                 xlab = "Elapsed Secs", ylab = "# sessions",
                 main = "Sessions Running") +

                 geom_point(aes(y = running, colour = "running"), size=2.0) +
                 geom_line( aes(y = running, colour = "running"), size=0.1) +

                 scale_colour_manual("Starts", values = c("#000000"))

plot_sessions_started <- qplot(elapsed, starts / window, data = b$sessions,
                 geom = c("point"),
                 xlab = "Elapsed Secs", ylab = opt$ylabel1stgraph,
                 main = "Sessions Started") +

                 geom_point(aes(y = starts / window, colour = "starts"), size=2.0) +
                 geom_line( aes(y = starts / window, colour = "starts"), size=0.1) +

                 scale_colour_manual("Starts", values = c("#188125"))

# Setup common elements of the latency plots
latency_plot <- ggplot(b$latencies, aes(x = elapsed)) +
                   facet_grid(. ~ op) +
                   labs(x = "Elapsed Secs", y = "Latency (ms)")

# Plot median, mean and 95th percentiles
plot_median_mean <- latency_plot + labs(title = "Mean and Median Latency") +
            geom_smooth(aes(y = mean, color = "mean"), size=0.5) +
            geom_point(aes(y = mean, color = "mean"), size=2.0) +

            geom_smooth(aes(y = median, color = "median"), size=0.5) +
            geom_point(aes(y = median, color = "median"), size=2.0) +

            scale_colour_manual("Percentile", values = c("#FFA700", "#188125"))
            # scale_color_hue("Percentile",
            #                 breaks = c("X95th", "mean", "median"),
            #                 labels = c("95th", "Mean", "Median"))

# Plot median, mean and 95th percentiles
plot_95 <- latency_plot + labs(title = "95th Percentile Latency") +
            geom_smooth(aes(y = X95th, color = "95th"), size=0.5) +
            geom_point(aes(y = X95th, color = "95th"), size=2.0) +

            scale_colour_manual("Percentile", values = c("#FF665F", "#009D91"))
            # scale_color_hue("Percentile",
            #                 breaks = c("X95th", "mean", "median"),
            #                 labels = c("95th", "Mean", "Median"))

# Plot 99th percentile
plot_99 <- latency_plot + labs(title = "99th Percentile Latency") +
            geom_smooth(aes(y = X99th, color = "99th"), size=0.5) +
            geom_point(aes(y = X99th, color = "99th"), size=2.0) +
            scale_colour_manual("Percentile", values = c("#FF665F", "#009D91"))
            # scale_color_hue("Percentile",
            #                 breaks = c("X99_9th","X99th" ),
            #                 labels = c("99.9th", "99th"))

# Plot 99.9th percentile
plot_999 <- latency_plot + labs(title = "99.9th Percentile Latency") +
            geom_smooth(aes(y = X99_9th, color = "99.9th"), size=0.5) +
            geom_point(aes(y = X99_9th, color = "99.9th"), size=2.0) +
            scale_colour_manual("Percentile", values = c("#FF665F", "#009D91", "#FFA700"))

# Plot 100th percentile
plot_max <- latency_plot + labs(title = "Maximum Latency") +
            geom_smooth(aes(y = max, color = "max"), size=0.5) +
            geom_point(aes(y = max, color = "max"), size=2.0) +
            scale_colour_manual("Percentile", values = c("#FF665F", "#009D91", "#FFA700"))

plot_upper_percentiles <- latency_plot + labs(title = "95th, 99th, 99.9th and 100th Percentile") +
            geom_smooth(aes(y = X95th, color = "95th"), size=0.5) +
            geom_point(aes(y = X95th, color = "95th"), size=2.0) +
            geom_smooth(aes(y = X99th, color = "99th"), size=0.5) +
            geom_point(aes(y = X99th, color = "99th"), size=2.0) +
            geom_smooth(aes(y = X99_9th, color = "99.9th"), size=0.5) +
            geom_point(aes(y = X99_9th, color = "99.9th"), size=2.0) +
            geom_smooth(aes(y = max, color = "max"), size=0.5) +
            geom_point(aes(y = max, color = "max"), size=2.0) +
            scale_colour_manual("Percentile", values = c("#FFFE00", "#FFA900", "#FF5600", "#FF0000"))

grid.newpage()

pushViewport(viewport(layout = grid.layout(6, 1)))

vplayout <- function(x,y) viewport(layout.pos.row = x, layout.pos.col = y)

print(plot_throughput, vp = vplayout(1,1))
print(plot_error, vp = vplayout(2,1))
print(plot_sessions_running, vp = vplayout(3,1))
print(plot_sessions_started, vp = vplayout(4,1))
print(plot_median_mean, vp = vplayout(5,1))
print(plot_upper_percentiles, vp = vplayout(6,1))

dev.off()
