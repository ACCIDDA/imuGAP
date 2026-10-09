# Makefile for imuGAP Inference & Offset Benchmarking Pipeline
# Manages intermediate artifacts (synthetic populations, compiled models, benchmark runs)

.PHONY: all help populations stress-test-pop strenuous-pop full-pop models stress-test-models strenuous-models full-models compile stress-test-compile strenuous-compile full-compile run stress-test-run strenuous-run full-run stress-test strenuous clean slurm-strenuous slurm-full slurm-stress-test merge-strenuous merge-full merge-stress-test

# Default target: display help
all: help

help:
	@echo "imuGAP Benchmarking Pipeline"
	@echo "============================"
	@echo "Available targets:"
	@echo "  make stress-test-pop     : Generate synthetic datasets for stress-test (config_stress_test.rds)"
	@echo "  make strenuous-pop       : Generate synthetic datasets for strenuous grid (config_strenuous.rds)"
	@echo "  make full-pop            : Generate synthetic datasets for full grid (config_full.rds)"
	@echo "  make populations         : Generate all population datasets (.rds from .yml)"
	@echo "  make stress-test-models  : Assemble intermediate Stan models for stress test in benchmodels/"
	@echo "  make strenuous-models    : Assemble intermediate Stan models for strenuous grid in benchmodels/"
	@echo "  make full-models         : Assemble all intermediate Stan models in benchmodels/"
	@echo "  make models              : Assemble all configured Stan models"
	@echo "  make stress-test-compile : Compile Stan models for stress test into benchobjs/ (cached)"
	@echo "  make strenuous-compile   : Compile Stan models for strenuous grid into benchobjs/ (cached)"
	@echo "  make full-compile        : Compile all Stan models into benchobjs/ (cached)"
	@echo "  make stress-test-run     : Run MCMC inference & compute metrics for stress test"
	@echo "  make strenuous-run       : Run MCMC inference & compute metrics for strenuous grid"
	@echo "  make full-run            : Run MCMC inference & compute metrics for full grid"
	@echo "  make stress-test         : Complete end-to-end stress test (populations -> compile -> run)"
	@echo "  make strenuous           : Complete end-to-end strenuous test (populations -> compile -> run)"
	@echo "  make slurm-strenuous     : Launch 55 SLURM array workers (4 cores) + dependent merge for strenuous"
	@echo "  make slurm-full          : Launch 55 SLURM array workers (4 cores) + dependent merge for full grid"
	@echo "  make slurm-stress-test   : Launch 55 SLURM array workers (4 cores) + dependent merge for stress test"
	@echo "  make merge-strenuous     : Consolidate partition files into final results for strenuous"
	@echo "  make merge-full          : Consolidate partition files into final results for full grid"
	@echo "  make merge-stress-test   : Consolidate partition files into final results for stress test"
	@echo "  make clean               : Remove intermediate populations, models, objects, parts, and results"

# --- 0. Stan Dependencies & Target Lists ---------------------------------------

STAN_TEMPLATE := ../../inst/stan/templates/bspline_static_offsets.stan.template
STAN_DEPS := $(shell find ../../inst/stan -type f -name "*.stan" -o -name "*.stan.template" 2>/dev/null)

UNIFIED_MODELS_STAN := benchmodels/model_unified_logit.stan benchmodels/model_unified_probit.stan
UNIFIED_MODELS_OBJS := benchobjs/model_unified_logit.rds benchobjs/model_unified_probit.rds

# --- 1. Synthetic Population Generation ---------------------------------------

# Pattern rule: Build %.rds from %.yml using benchmark_synthetic_populations.R
%.rds: benchmark_synthetic_populations.R %.yml
	Rscript $^

stress-test-pop: config_stress_test.rds

strenuous-pop: config_strenuous.rds

full-pop: config_full.rds

populations: config_stress_test.rds config_strenuous.rds config_full.rds

# --- 2. Model Template Generation & Compilation -------------------------------

benchmodels:
	mkdir -p benchmodels

benchobjs:
	mkdir -p benchobjs

# Pattern rule: Generate model_*.stan from template and compilation helper
benchmodels/model_%.stan: $(STAN_DEPS) benchmark_compilation.R | benchmodels
	Rscript -e 'source("benchmark_compilation.R"); generate_single_model_stan("$*")'

# Pattern rule: Compile model_*.rds from model_*.stan and all Stan include dependencies
benchobjs/model_%.rds: benchmodels/model_%.stan $(STAN_DEPS) | benchobjs
	Rscript -e 'source("benchmark_compilation.R"); compile_single_model("$*")'

stress-test-models: $(UNIFIED_MODELS_STAN)

strenuous-models: $(UNIFIED_MODELS_STAN)

full-models: $(UNIFIED_MODELS_STAN)

models: stress-test-models strenuous-models full-models

stress-test-compile: $(UNIFIED_MODELS_OBJS)

strenuous-compile: $(UNIFIED_MODELS_OBJS)

full-compile: $(UNIFIED_MODELS_OBJS)

compile: stress-test-compile

# Standalone compilation targets (legacy / un-unified)
standalone-stress-compile:
	Rscript benchmark_compilation.R config_stress_test.yml --standalone --compile --parallel

standalone-strenuous-compile:
	Rscript benchmark_compilation.R config_strenuous.yml --standalone --compile --parallel

standalone-full-compile:
	Rscript benchmark_compilation.R config_full.yml --standalone --compile --parallel

# --- 3. Inference Execution & Local Runs ---------------------------------------

results_stress_test.rds: config_stress_test.rds $(UNIFIED_MODELS_OBJS) benchmark_inference_runner.R
	Rscript benchmark_inference_runner.R config_stress_test.rds $@

results_strenuous.rds: config_strenuous.rds $(UNIFIED_MODELS_OBJS) benchmark_inference_runner.R
	Rscript benchmark_inference_runner.R config_strenuous.rds $@

results_full_grid.rds: config_full.rds $(UNIFIED_MODELS_OBJS) benchmark_inference_runner.R
	Rscript benchmark_inference_runner.R config_full.rds $@

stress-test-run: results_stress_test.rds

strenuous-run: results_strenuous.rds

full-run: results_full_grid.rds

run: stress-test-run

stress-test: stress-test-pop stress-test-compile stress-test-run

strenuous: strenuous-pop strenuous-compile strenuous-run

# --- 4. SLURM Distributed Execution & Consolidation ---------------------------

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

# --- 5. Clean Target ----------------------------------------------------------

clean:
	rm -rf config_stress_test.rds config_strenuous.rds config_full.rds benchmodels benchobjs results_*.rds results_*.csv results_parts logs test_output.log
