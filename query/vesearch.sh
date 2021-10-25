
cd C:\daniel.vaulot@gmail.com\Shiny\metapr2\sandbox

.\vsearch --usearch_global query.fasta --db metapr2_asv_5sets_Eukaryota.fasta --dbmatched results.fasta --fastapairs align.fasta --id 0.90 --iddef 2 --maxaccepts 10 --top_hits_only --uc results.tsv