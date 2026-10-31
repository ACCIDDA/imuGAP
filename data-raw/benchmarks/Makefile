# Makefile for imuGAP Inference & Offset Benchmarking Pipeline
# Manages intermediate artifacts (synthetic populations, compiled models, benchmark runs)

.PHONY: all help populations \
	stress-test-pop strenuous-pop full-pop \
	compile run \
	stress-test-run strenuous-run full-run \
	stress-test strenuous full \
	figures \
	slurm-stress-test slurm-strenuous slurm-full \
	merge-stress-test merge-strenuous merge-full \
	clean

# Generic Rscript execution macro: $^ (script + dependencies) $(1) (flags) $@ (target)
R = $(strip Rscript $^ $(1) $@)

# Default target: display help
all: help

help:
	@echo "imuGAP Benchmarking Pipeline"
	@echo "============================"
	@echo "Configuration tiers (%):"
	@echo "  stress-test : Fast sanity check on bounding grid (K in {3, 10}, sigma in {0.2, 1.4})"
	@echo "  strenuous   : Algorithmic stress boundaries (K in {3, 10, 100}, sigma in {0.6, 1.4})"
	@echo "  full        : Complete factorial production grid (4 K x 4 sigma x 19 p0, N=300)"
	@echo ""
	@echo "Tier targets (where % is stress-test, strenuous, or full):"
	@echo "  make %-pop               : Generate synthetic datasets for tier (config_%.rds)"
	@echo "  make %-run               : Run MCMC inference & compute metrics"
	@echo "  make %                   : Complete end-to-end pipeline (%-pop -> compile -> %-run)"
	@echo "  make slurm-%             : Launch SLURM array workers (4 cores) + dependent merge"
	@echo "  make merge-%             : Consolidate partition files into final results"
	@echo ""
	@echo "Shared model & utility targets:"
	@echo "  make compile             : Compile unified Stan models into benchobjs/ (cached)"
	@echo "  make populations         : Generate all population datasets (.rds from .yml)"
	@echo "  make figures             : Generate diagnostic and benchmark plots from results"
	@echo "  make clean               : Remove intermediate datasets, objects, and results"

# --- 0. Stan Dependencies & Target Lists ---------------------------------------

STAN_DEPS := $(shell find ../../inst/stan -type f -name "*.stan" 2>/dev/null)

UNIFIED_MODELS_OBJS := benchobjs/model_unified_logit.rds benchobjs/model_unified_probit.rds

# --- 1. Synthetic Population Generation ---------------------------------------

# Pattern rule: Build %.rds from %.yml using benchmark_synthetic_populations.R
%.rds: benchmark_synthetic_populations.R %.yml
	$(call R)

stress-test-pop: config_stress_test.rds

strenuous-pop: config_strenuous.rds

full-pop: config_full.rds

populations: config_stress_test.rds config_strenuous.rds config_full.rds

# --- 2. Model Compilation -----------------------------------------------------

benchobjs:
	mkdir -p benchobjs

# Pattern rule: Compile unified Stan model object with Stan thread support
benchobjs/model_unified_%.rds: benchmark_compilation.R \
	../../inst/stan/bspline_static_dispatch_dispatch_%.stan \
	$(STAN_DEPS) | benchobjs
	Rscript $(wordlist 1, 2, $^) $@

compile: $(UNIFIED_MODELS_OBJS)

stress-test-compile strenuous-compile full-compile: compile

results_parts:
	mkdir -p results_parts

logs:
	mkdir -p logs


# --- 3. Inference Execution & Local Runs ---------------------------------------

results_stress_test.rds: benchmark_inference_runner.R config_stress_test.rds \
	$(UNIFIED_MODELS_OBJS)
	$(call R,--save-profiles)

results_strenuous.rds: benchmark_inference_runner.R config_strenuous.rds \
	$(UNIFIED_MODELS_OBJS)
	$(call R)

results_full_grid.rds: benchmark_inference_runner.R config_full.rds \
	$(UNIFIED_MODELS_OBJS)
	$(call R)

stress-test-run: results_stress_test.rds

strenuous-run: results_strenuous.rds

full-run: results_full_grid.rds

run: stress-test-run

stress-test: stress-test-pop compile stress-test-run figures

strenuous: strenuous-pop compile strenuous-run

full: full-pop compile full-run

# --- 4. Benchmark Visualizations & Diagnostic Figures -------------------------

figures: figures/fig_error_strata.png

figures/fig_error_strata.png: benchmark_visualize.R results_stress_test.rds
	@mkdir -p $(@D)
	$(call R)

# --- 5. SLURM Distributed Execution & Consolidation ---------------------------

slurm-strenuous:
	bash slurm_submit.sh strenuous 4 4 300 150

slurm-full:
	bash slurm_submit.sh full 4 4 300 150

slurm-stress-test:
	bash slurm_submit.sh stress_test 4 4 300 150

merge-strenuous:
	Rscript benchmark_merge.R strenuous results_parts

merge-full:
	Rscript benchmark_merge.R full results_parts

merge-stress-test:
	Rscript benchmark_merge.R stress_test results_parts

# --- 6. Clean Target ----------------------------------------------------------

clean:
	rm -rf config_stress_test.rds config_strenuous.rds config_full.rds \
		benchobjs results_*.rds results_*.csv results_parts logs test_output.log figures
