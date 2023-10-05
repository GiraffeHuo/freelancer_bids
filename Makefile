report: ./writeup/compile_report.R ./writeup/report.Rmd
	@./writeup/compile_report.R
	@rm ./writeup/report.md
	@echo "-------------------------------------------------------------"
	@echo "                           FIN                               "
	@echo "-------------------------------------------------------------"

freelancer_bids_code:
	@git clone https://bitbucket.techops.usw2.upwork/scm/pmmd/freelancer_bids.git
	@mv freelancer_bids etl
	@rm -rf etl/.git
	@rm -rf etl/.gitignore
	@rm -rf etl/README.md
	@rm -rf etl/makefile_map.png