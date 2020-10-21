*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : GERA_DIS.PRG
* Data        : 01/01/99
* Objetivo    : GERACAO DE DISQUETE COM PRODUTO
* PRG"s       : INICIO
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION GERA_DIS()
SELE 1
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
* Selecao do produto
DO WHILE .T.
   PLIMPA()
   SET COLOR TO W/N,+W/R
   eCLAS_PRO = 0
   eTIPO_PRO = 0
   @ 05,02 SAY "Produto : "
   GO TOP
   DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1]="@R99999"
   PIC[2]="@R99999"
   PIC[3]="@S40"
   CAB[1]="    "
   CAB[2]="    "
   CAB[3]="    "
   DBEDIT(5,15,5,70,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
   eCLAS_PRO = CODIGO
   eTIPO_PRO = TIPO
   EXIT
ENDDO
PLQUA("01")   
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   PLQUA("01")
   PMENSAG("Aguarde ...")
   PPARA()
   sisDADOS = sisDADOS+"TEMP\"
   SET EXCLUSIVE ON
   SELE 1
   PLQUA("01")
   PMENSAG("Gerando Arquivo : CONTATO.DBF"
   USE CONTATO
   GOTO TOP
   eTEMP = sisDADOS+"CONTATO.DBF"
   COPY TO &eTEMP
   USE
   PLQUA("01")
   PMENSAG("Gerando Arquivo : CLAS_PRO.DBF"
   USE CLAS_PRO
   GOTO TOP
   eTEMP = sisDADOS+"CLAS_PRO.DBF"
   COPY TO &eTEMP
   USE
   PLQUA("01")
   PMENSAG("Gerando Arquivo : CAD_QUAD.DBF"
   USE CAD_QUAD
   GOTO TOP
   eTEMP = sisDADOS+"CAD_QUAD.DBF"
   COPY TO &eTEMP 
   USE
   PLQUA("01")
   PMENSAG("Gerando Arquivo : CAD_APAR.DBF"
   USE CAD_APAR
   GOTO TOP
   eTEMP = sisDADOS+"CAD_APAR.DBF"
   COPY TO &eTEMP 
   USE
   PLQUA("01")
   PMENSAG("Gerando Arquivo : UNI_IMOB.DBF"
   USE UNI_IMOB
   GOTO TOP
   eTEMP = sisDADOS+"UNI_IMOB.DBF"
   COPY TO &eTEMP 
   USE
   PLQUA("01")
   PMENSAG("Gerando Arquivo : CAD_VEND.DBF"
   USE CAD_VEND
   GOTO TOP
   eTEMP = sisDADOS+"CAD_VEND.DBF"
   COPY TO &eTEMP 
   USE
   PLQUA("01")
   PMENSAG("Gerando Arquivo : CAD_QUIT.DBF"
   USE CAD_QUIT 
   GOTO TOP
   eTEMP = sisDADOS+"CAD_QUIT.DBF"
   COPY TO &eTEMP 
   USE
   PLQUA("01")
   PMENSAG("Gerando Arquivo : INDICES.DBF"
   USE INDICES
   GOTO TOP
   eTEMP = sisDADOS+"INDICES.DBF"
   COPY TO &eTEMP 
   USE
   PLQUA("01")
   PMENSAG("Gerando Arquivo : TAXA_MES.DBF"
   USE TAXA_MES                     
   GOTO TOP
   eTEMP = sisDADOS+"TAXA_MES.DBF"
   COPY TO &eTEMP 
   USE
   PLQUA("01")
   PMENSAG("Gerando Arquivo : CORRETOR.DBF"
   USE CORRETOR 
   GOTO TOP
   eTEMP = sisDADOS+"CORRETOR.DBF"
   COPY TO &eTEMP 
   USE
   SELE 1
   USE CAD_QUAD INDEX CAD_QUAD
   SET FILTER TO CADASTRO .AND. NUM_LOTEAM = STR(eCLAS_PRO,5)
   GOTO TOP
   DO WHILE .NOT. EOF()
      eZERO = "000"
      eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(NUM_LOTEAM))))+ALLTRIM(NUM_LOTEAM)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(NUM_QUADRA))))+ALLTRIM(NUM_QUADRA) 
      SELE 2
      USE &eARQ
      eTEMP = sisDADOS+eARQ
      PLQUA("01")
      PMENSAG("Gerando Arquivo : "+eTEMP
      COPY TO &eTEMP 
      USE
      SELE 1
      SKIP
   ENDDO
   SELE 1
   USE CAD_APAR INDEX CAD_APAR
   SET FILTER TO CADASTRO .AND. NUM_LOTEAM = STR(eCLAS_PRO,5)
   GOTO TOP
   DO WHILE .NOT. EOF()
      eZERO = "000"
      eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(NUM_LOTEAM))))+ALLTRIM(NUM_LOTEAM)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(NUM_BLOCO))))+ALLTRIM(NUM_BLOCO) 
      SELE 2
      USE &eARQ
      eTEMP = sisDADOS+eARQ
      PLQUA("01")
      PMENSAG("Gerando Arquivo : "+eTEMP
      COPY TO &eTEMP 
      USE
      SELE 1
      SKIP
   ENDDO
   SELE 1
   USE
   SELE 2
   USE
   SELE 1
   USE
   SET EXCLUSIVE OFF 
ENDIF
CLOSE ALL
RETURN nil
********************* fim *********************************



