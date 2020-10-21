//Created by HMG Data Base Assistant v.8.6.12 (28.01.2016-17:53:05)
//Under structure of CLASSE_R.DBF

Function _Create_CLASSE_R()
     Local aStructure := {;
                         { "CODIGO"     ,  "N"  ,   12   ,   0    },;
                         { "DESCRICAO"  ,  "C"  ,   45   ,   0    },;
                         { "CONTA_CONT" ,  "C"  ,   15   ,   0    };
                         }
     DBCreate( "CLASSE_R.DBF" , aStructure )

Return Nil