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
  
    B_9_2 [label=', index_cool('B',9,2,'(x)'), ']
    B_9_1 [label=', index_cool('B',9,1,'(x)'), ']
    B_10_1 [label=', index_cool('B',10,1,'(x)'), ']
    B_9_0 [label=', index_cool('B',9,0,'(x)'), ']
    B_10_0 [label=', index_cool('B',10,0,'(x)'), ']
    B_11_0 [label=', index_cool('B',11,0,'(x)'), ']
    B_9_base [label=', index_cool('B',9,-1,'(x)'), ']
    B_10_base [label=', index_cool('B',10,-1,'(x)'), ']
    B_11_base [label=', index_cool('B',11,-1,'(x)'), ']
    B_12_base [label=', index_cool('B',12,-1,'(x)'), ']
  
    B_9_2 -> B_9_1 [label=', index_cool('ω',9,2), ']
    B_9_2 -> B_10_1 [label=', index_cool('1−ω',9,2), ']
    B_9_1 -> B_9_0 [label=', index_cool('ω',9,1), ']
    B_9_1 -> B_10_0 [label=', index_cool('1−ω',9,1), ']
    B_10_1 -> B_10_0 [label=', index_cool('ω',10,1), ']
    B_10_1 -> B_11_0 [label=', index_cool('1−ω',10,1), ']
    B_9_0 -> B_9_base [label=', index_cool('ω',9,0), ']
    B_9_0 -> B_10_base [label=', index_cool('1−ω',9,0), ']
    B_10_0 -> B_10_base [label=', index_cool('ω',10,0), ']
    B_10_0 -> B_11_base [label=', index_cool('1−ω',10,0), ']
    B_11_0 -> B_11_base [label=', index_cool('ω',11,0), ']
    B_11_0 -> B_12_base [label=', index_cool('1−ω',11,0), ']
  }
'))
