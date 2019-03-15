query:
	cd .data/; \
	export R_WYNTONQUERY_EXCLUDE="$$(cut -f 1 host_table.tsv | tail -n +4) qb3-hmid1"; \
	echo $$R_WYNTONQUERY_EXCLUDE | wc -w; \
	qsub -v R_WYNTONQUERY_EXCLUDE="$$R_WYNTONQUERY_EXCLUDE" ../inst/scripts/system_info.sge

update:
	cd .data/; \
	Rscript merge_all.R
	cp .data/host_table.tsv ../wynton/docs/assets/data/
