SHELL=/usr/bin/bash

EXCLUDES="cc-idgpu2 qb3-hmid1 qb3-id102"

known_hostnames.txt: host_table.tsv
	cut -f 1 $< | tail -n +6 > $@

hostnames_to_skip.txt: known_hostnames.txt
	cat $< > $@.tmp
	echo "$(R_WYNTONQUERY_EXCLUDE)" >> $@.tmp
	sort -u $@.tmp > $@
	rm $@.tmp

debug: hostnames_to_skip.txt
	@export R_WYNTONQUERY_EXCLUDE=($$(cat "$<")); \
	printf "Hostnames to skip: [n=%d] %s\n" "$${#R_WYNTONQUERY_EXCLUDE[@]}" "$${R_WYNTONQUERY_EXCLUDE[*]}"

query: hostnames_to_skip.txt
	export R_WYNTONQUERY_EXCLUDE=$(cat "$@")
#	qsub -v R_WYNTONQUERY_EXCLUDE="$R_WYNTONQUERY_EXCLUDE" ../inst/scripts/system_info.sge

merge: host_table.tsv ../../wynton/docs/assets/data/host_table.tsv
	Rscript merge_all.R

diff: host_table.tsv
	diff -u <(cut -f 1-2 ../../wynton/docs/assets/data/$<) <(cut -f 1-2 $<)

deploy: host_table.tsv
	cp "$<" ../../wynton/docs/assets/data/

cleanup:
	rm "known_hostnames.txt" "hostnames_to_skip.txt"

