all: \
	lint \
	tests \
	coverage

define lint
	R -e "library(lintr)" \
	  -e "lint_dir('R', linters = with_defaults(line_length_linter(100)))" \
	  -e "lint_dir('tests/testthat', linters = with_defaults(line_length_linter(100)))"
endef

.PHONY: all check clean coverage linter results tests

results/xTable_262_2021.csv:
	mkdir --parents $(@D)
	Rscript -e "source('src/calculate_xPoint.R')"

results: src/calculate_xGoal.R
	mkdir --parents $(@D)
	Rscript -e "source('src/calculate_xGoal.R')"

check:
	R -e "library(styler)" \
	  -e "resumen <- style_dir('tests')" \
	  -e "resumen <- rbind(resumen, style_dir('R'))" \
	  -e "any(resumen[[2]])" \
	  | grep FALSE

clean:
	rm --force NAMESPACE
	rm --force --recursive xGoal.Rcheck
	rm --force *.tar.gz

coverage:
	Rscript coverage.R

format:
	R -e "library(styler)" \
	  -e "style_dir('tests')" \
	  -e "style_dir('R')"

linter:
	$(lint)
	$(lint) | grep -e "\^" && exit 1 || exit 0

tests:
	R -e "testthat::test_dir('tests/testthat/', report = 'summary', stop_on_failure = TRUE)"

setup:
	R -e "devtools::document()" && \
	R CMD build . && \
	R CMD check xGoal_21.08.27.tar.gz && \
	R CMD INSTALL xGoal_21.08.27.tar.gz

setup_github:
	sed "s/workspaces\/calculator-trs/workdir/g" tests/testthat/test_xGoal.R > tmpfile; mv tmpfile tests/testthat/test_xGoal.R
	sed "s/workspaces\/calculator-trs/workdir/g" tests/testthat/test_xTable.R > tmpfile; mv tmpfile tests/testthat/test_xTable.R
	
presition_premier_league:
	Rscript src/presition_of_multinom.R --league-season=39_2021

update_serie_a:
	Rscript src/calculate_xPoint.R --league-season=135_2021
	Rscript src/add_winner_to_league.R --league-season=135_2021

update_ligue_1:
	Rscript src/calculate_xPoint.R --league-season=61_2021
	Rscript src/add_winner_to_league.R --league-season=61_2021

update_eredivisie:
	Rscript src/calculate_xPoint.R --league-season=88_2021
	Rscript src/add_winner_to_league.R --league-season=88_2021

update_la_liga:
	Rscript src/calculate_xPoint.R --league-season=140_2021
	Rscript src/add_winner_to_league.R --league-season=140_2021

update_premier_league:
	mkdir --parents results/
	Rscript src/calculate_xPoint.R --league-season=39_2021
	Rscript src/add_winner_to_league.R --league-season=39_2021

update_bundesliga:
	Rscript src/calculate_xPoint.R --league-season=78_2021
	Rscript src/add_winner_to_league.R --league-season=78_2021

update_primeira_liga:
	Rscript src/calculate_xPoint.R --league-season=94_2021
	Rscript src/add_winner_to_league.R --league-season=94_2021

update: \
	update_primeira_liga \
	update_bundesliga \
	update_premier_league \
	update_la_liga \
	update_eredivisie \
	update_ligue_1 \
	update_serie_a