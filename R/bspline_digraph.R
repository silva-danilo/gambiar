# packs
library(DiagrammeR)

# prep
index_cool <- function(letter, low, upp, arg=""){
  paste0('< <TABLE BORDER="0" CELLSPACING="0" CELLPADDING="0"><TR><TD ROWSPAN="2">',
         letter, 
         '</TD><TD><FONT POINT-SIZE="9">',
         upp, 
         '</FONT></TD><TD ROWSPAN="2">',
         arg,
         '</TD></TR><TR><TD><FONT POINT-SIZE="9">',
         low, 
         '</FONT></TD></TR></TABLE> >')
}

# plot
grViz(paste0('
  digraph boxes_and_circles{
  
    graph [rankdir=LR, overlap=true, splines=line] 
  
    node [shape=circle, fixedsize=true, width=0.8, height=0.8, fontname=Helvetica]
  
    N_9_3 [label=', index_cool('N',9,3,'(x)'), ']
    N_9_2 [label=', index_cool('N',9,2,'(x)'), ']
    N_10_2 [label=', index_cool('N',10,2,'(x)'), ']
    N_9_1 [label=', index_cool('N',9,1,'(x)'), ']
    N_10_1 [label=', index_cool('N',10,1,'(x)'), ']
    N_11_1 [label=', index_cool('N',11,1,'(x)'), ']
    N_9_0 [label=', index_cool('N',9,0,'(x)'), ']
    N_10_0 [label=', index_cool('N',10,0,'(x)'), ']
    N_11_0 [label=', index_cool('N',11,0,'(x)'), ']
    N_12_0 [label=', index_cool('N',12,0,'(x)'), ']
  
    N_9_3 -> N_9_2 [label=', index_cool('ω',9,3), ']
    N_9_3 -> N_10_2 [label=', index_cool('1−ω',9,3), ']
    N_9_2 -> N_9_1 [label=', index_cool('ω',9,2), ']
    N_9_2 -> N_10_1 [label=', index_cool('1−ω',9,2), ']
    N_10_2 -> N_10_1 [label=', index_cool('ω',10,2), ']
    N_10_2 -> N_11_1 [label=', index_cool('1−ω',10,2), ']
    N_9_1 -> N_9_0 [label=', index_cool('ω',9,1), ']
    N_9_1 -> N_10_0 [label=', index_cool('1−ω',9,1), ']
    N_10_1 -> N_10_0 [label=', index_cool('ω',10,1), ']
    N_10_1 -> N_11_0 [label=', index_cool('1−ω',10,1), ']
    N_11_1 -> N_11_0 [label=', index_cool('ω',11,1), ']
    N_11_1 -> N_12_0 [label=', index_cool('1−ω',11,1), ']
  }
'))