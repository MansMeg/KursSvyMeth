data agrarstr;
infile 's�kv�g\agstrat.dat' delimiter=',' firstobs=2;
input county $ state $ acres92 acres87 acres82 farms92 farms87 farms82 large92 large87 large82 small92 small87 small82 region $ rn weight;
run;
proc print data=agrarstr;
run;
