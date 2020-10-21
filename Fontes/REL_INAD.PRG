*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_INAD.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE INADIMPLENTES 
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_INAD()
SAVE SCREEN TO TELA03
PTIT( "RELAT‡RIO DE INADIMPLENTES")
SELE 1
USE UNI_IMOB INDEX UNI_IMOB
GOTO TOP
SELE 2
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
SELE 3
USE CAD_QUAD INDEX CAD_QUAD
GOTO TOP
SELE 4
USE CAD_APAR INDEX CAD_APAR
GOTO TOP
SELE 5
USE CONTATO INDEX NUM_CLIE,CONTATO
GOTO TOP
SELE 6
USE CAD_VEND INDEX CAD_VEND,DATA_VEN
GOTO TOP
SELE 8
* Cadastro de Parcelas
SELE 9
USE MSG_BOLE INDEX MSG_PROD,MSG_BOLE
GOTO TOP
SELE 2
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
   eDESC_PRO = DESCRICAO
   EXIT
ENDDO
* Selecao da rotina de Alteracao conforme o tipo de produto
eQUADRA = SPACE(1)
DO CASE
   CASE eTIPO_PRO = 1
      * Consulta de Loteamentos
      SELE 3
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA     = SPACE(5)
      eNUM_IMOVEL = SPACE(5)
      PLQUA("01")
      PPERG ( "Deseja Selecionar "+sisGRUPO1+" (S/N) ? ")
      IF sisRESP = "S"
         @ 06,02 SAY sisGRUPO1+" : "
         GO TOP 
         DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
         AFIELDS(CAMPO,.T.,LENS)
         PIC[1]="@R99999"
         PIC[2]="@R99999"
         PIC[3]="@R99999"
         PIC[4]="@S99999"
         PIC[5]="@S1"  
         PIC[6]="@S20"         
         CAB[1]="    "
         CAB[2]="    "
         CAB[3]="    "
         CAB[4]="    "
         CAB[5]=" "   
         CAB[6]="    "        
         DBEDIT(6,14,6,75,CAMPO,"FUNC_CONS",PIC,CAB," "," - "," ")
         eQUADRA     = NUM_QUADRA
         eNUM_INICIO = NUM_INICIO
         eNUM_FINAL  = NUM_FINAL
      ENDIF
   CASE eTIPO_PRO = 2
      * Consulta de Apartamentos
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA    = SPACE(5)
      eNUM_IMOVEL= SPACE(5)
      PLQUA("01")
      PPERG ( "Deseja Selecionar "+sisGRUPO2+" (S/N) ? ")
      IF sisRESP = "S"
         @ 06,02 SAY sisGRUPO2+" : "
         GO TOP
         DECLARE CAMPO[4],PIC[4],CAB[4],LENS[4]
         AFIELDS(CAMPO,.T.,LENS)
         PIC[1]="@R99999"
         PIC[2]="@R99999"
         PIC[3]="@S30"
         PIC[4]="@R99999"
         CAB[1]="    "
         CAB[2]="    "
         CAB[3]="    "
         CAB[4]="    "
         DBEDIT(6,08,6,76,CAMPO,"FUNC_CONS",PIC,CAB," "," - "," ")
         eQUADRA      = NUM_BLOCO
         eQUANTIDADE = QUANTIDADE
      ENDIF
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
eARQUIVO = SPACE(60)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"INADIMPLENTES.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "INADIMPLENTES.TXT"+SPACE(10) 
ENDIF     
eSAIDA   = SPACE(10)
DO WHILE .T.
   PLQUA("01")
   PPERG ( "Deseja extrato em arquivo texto no Drive (X:) (S/N) ? ")
   IF sisRESP = "S"
      PLQUA("01")
	  @ 23,00 SAY "CONFIRME O DESTINO  : " GET eARQUIVO
      READ
      eARQUIVO = ALLTRIM(eARQUIVO)
      PLQUA("01")
	  PMENSAG("Insira um disco no drive (X:), e tecle ENTER.")
      PPARA()
      eSAIDA = "ARQUIVO"
      EXIT
   ELSE
      PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
      PPARA()
      DO PIMPR
      eSAIDA = "IMPRESSORA"
      EXIT
   ENDIF
ENDDO
eDATA_INI   = CTOD("01/01/1980")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   @ 08,02 SAY "Periodo : " GET eDATA_INI
   @ 08,25 SAY "Ate : "     GET eDATA_FIM
   READ
   IF eDATA_INI = CTOD("") .OR. eDATA_FIM = CTOD("")
      PLQUA("01")
      PMSGR( "Datas Inv†lidas !",3)
      LOOP
   ELSE
      IF eDATA_INI > eDATA_FIM
         PLQUA("01")
         PMSGR( "Data Inicio > Que Final !",3)
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDIF
ENDDO
eTIPO = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Tipo de Parcelas (S)inal,(N)ormais,(I)ntermediarias ou (A)mbas ? " GET eTIPO PICT "@!X" VALID eTIPO $("SNIA")
READ
eINTER_INI = 0
eINTER_FIM = 0
PLQUA("01")
PPERG ( "Deseja Definir um Intervalo de Parcelas ? ")
IF sisRESP = "S"
   DO WHILE .T.
      PLQUA("01")
      @ 23,00 SAY "Parcela de Inicio : " GET eINTER_INI PICT "@r 999"
      @ 23,35 SAY "Parcela de Fim : " GET eINTER_FIM PICT "@r 999"
      READ
      IF eINTER_FIM < eINTER_INI
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDDO
ENDIF
IF eQUADRA <> SPACE(5)
   SELE 8
   eIND_CLAS = STR(eCLAS_PRO,5)
   eIND_QUAD = eQUADRA
   eZERO = "000"
   eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   USE &eARQ INDEX &eIND1,&eIND2,&eIND3
   SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA
   GOTO TOP
ELSE
   PLQUA("01")
   PMSGR( "Atená∆o !",3)
   PMENSAG("Aguarde a montagem do Banco de Dados para Impress∆o ...")
   IF eTIPO_PRO = 1
      SELE 8
      SET EXCLUSIVE ON
      USE TEMP_PAR
      IF F_BLOQA()
         ZAP
         UNLOCK
      ENDIF
      SET EXCLUSIVE OFF
      SELE 3
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO 
      GOTO TOP
      DO WHILE .NOT. EOF()
         eIND_CLAS = STR(eCLAS_PRO,5)
         eIND_QUAD = NUM_QUADRA
         SELE 8
         USE TEMP_PAR
         GOTO TOP
         eZERO = "000"
         eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA
         GOTO TOP
         SELE 3
         SKIP
      ENDDO
      SET EXCLUSIVE ON
      SELE 8
      USE TEMP_PAR
      INDEX ON CLASSE+QUADRA+NUM_IMOVEL+STR(PARTE,1) TO TEMP_PAR
      GOTO TOP
      USE TEMP_PAR INDEX TEMP_PAR
      GOTO TOP
      SET EXCLUSIVE OFF
   ELSE
      SELE 8
      SET EXCLUSIVE ON
      USE TEMP_PAR
      IF F_BLOQA()
         ZAP
         UNLOCK
      ENDIF
      SET EXCLUSIVE OFF
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO 
      GOTO TOP
      DO WHILE .NOT. EOF()
         eIND_CLAS = STR(eCLAS_PRO,5)
         eIND_QUAD = NUM_BLOCO
         SELE 8
         USE TEMP_PAR
         GOTO TOP
         eZERO = "000"
         eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA
         GOTO TOP
         SELE 4
         SKIP
      ENDDO
      SET EXCLUSIVE ON
      SELE 8
      USE TEMP_PAR
      INDEX ON CLASSE+QUADRA+NUM_IMOVEL+STR(PARTE,1) TO TEMP_PAR
      GOTO TOP
      USE TEMP_PAR INDEX TEMP_PAR
      GOTO TOP
      SET EXCLUSIVE OFF
   ENDIF
ENDIF
* Intervalo de Parcelas
IF eINTER_INI <> 0 .AND. eINTER_FIM <> 0
   SELE 8
   GOTO TOP
   eCONTADOR   = 0
   eCLASSE     = CLASSE
   eQUADRA     = QUADRA
   eNUM_IMOVEL = NUM_IMOVEL
   ePARTE      = PARTE
   DO WHILE .NOT. EOF()
      IF eCLASSE = CLASSE .AND. eQUADRA = QUADRA .AND. eNUM_IMOVEL = NUM_IMOVEL .AND. ePARTE = PARTE
         eCONTADOR = eCONTADOR + 1
         SKIP
      ELSE
         IF eCONTADOR < eINTER_INI .OR. eCONTADOR > eINTER_FIM 
            SEEK eCLASSE+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
            DO WHILE (eCLASSE = CLASSE .AND. eQUADRA = QUADRA .AND. eNUM_IMOVEL = NUM_IMOVEL .AND. ePARTE = PARTE) .AND. .NOT. EOF()
               DELETE
               SKIP
            ENDDO
            eCONTADOR = 0
         ELSE
            eCONTADOR = 0
         ENDIF
         eCLASSE     = CLASSE
         eQUADRA     = QUADRA
         eNUM_IMOVEL = NUM_IMOVEL
         ePARTE      = PARTE
      ENDIF
   ENDDO
   SELE 8
   GOTO TOP
   PACK
   GOTO TOP
ENDIF
DO CASE
   CASE eTIPO = "A"
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL
   CASE eTIPO = "N"
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL ;
      .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
   CASE eTIPO = "I" 
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL ;
      .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
   CASE eTIPO = "S" 
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL ;
     .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
ENDCASE
SELE 8
GOTO TOP
IF EOF()
   PLQUA("01")
   PMSGR( "Atená∆o !",3)
   PMENSAG("N∆o H† Devedores Neste Per°odo !")
   PPARA()
ELSE
   IF eSAIDA = "ARQUIVO"
      SET PRINT ON
      SET DEVICE TO PRINT
      SET PRINTER TO &eARQUIVO
   ELSE
      PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
      PPARA()
      DO PIMPR
      SET DEVICE TO PRINT
   ENDIF
   eDATA_PGTO = DATA_PGTO
   DECLARE vDEVEDOR[11],vVLR_DEVE[11]
   AFILL(vDEVEDOR,0)
   AFILL(vVLR_DEVE,0)
   eTOT_PARC  = 0
   eSUB_PARC  = 0
   eTOT_NPAR  = 0
   eNUM_ANTE  = 0
   DO WHILE .NOT. EOF()
      @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
      @ 01,120 SAY "Data : "+dtoc(DATE())
      @ 02,00  SAY "Sistema : "+sisTIT2
      @ 02,121 SAY "Relat: SAN004"
      @ 03,30  SAY sisLETRAE+"RELATORIO INADIMPLENTES ATE "+DTOC(eDATA_FIM)+sisLETRAC
      @ 04,00  SAY "    Produto    : "+STR(eCLAS_PRO,5)+" - "+ALLTRIM(eDESC_PRO)+IIF(eQUADRA = SPACE(5)," ",+" - "+ALLTRIM(eDESC_PRO)+" - Quadra\Bloco : "+ALLTRIM(eQUADRA))
      @ 05,00  SAY REPLICATE("=",135)
      @ 06,02  SAY "No. IMOVEL"
      @ 06,15  SAY "PROPRIETARIO"
      @ 06,65  SAY "DATA VENC."
      @ 06,77  SAY "TIPO"
      @ 06,85  SAY "NUM.PARC."
      @ 06,97  SAY "VLR. PARCELA"
      @ 06,115 SAY "OBS"
      @ 07,00 SAY REPLICATE("=",135)
      LIN = 8
      DO WHILE .NOT. EOF() .AND. LIN <= 54
         @ LIN,02  SAY ALLTRIM(QUADRA)+"/"+ALLTRIM(NUM_IMOVEL)+"/"+STR(PARTE,1)
         eQUADRA     = QUADRA
         eNUM_IMOVEL = NUM_IMOVEL
         ePARTE      = PARTE
         SELE 6
         GOTO TOP
         SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
         eNUM_CLIE = NUM_CLIE
         SELE 5
         GOTO TOP
         SEEK eNUM_CLIE
         @ LIN,15  SAY ALLTRIM(SUBSTR(CLIENTE,1,30))+" - "+ALLTRIM(SUBSTR(CPF_CGC,1,17))
         SELE 8
         @ LIN,65  SAY DATA_VENC
         @ LIN,79  SAY TIPO_PARC
         @ LIN,86  SAY ALLTRIM(NUM_PARC)
         @ LIN,94  SAY VLR_PARC   PICT "@R 999,999,999,999.99"
         @ LIN,115 SAY OBS
         eSUB_PARC  = eSUB_PARC  + VLR_PARC
         eTOT_NPAR = eTOT_NPAR + 1
         LIN = LIN + 1
         SKIP
         IF STR(eCLAS_PRO,5) <> CLASSE .OR. eQUADRA <> QUADRA .OR. eNUM_IMOVEL <> NUM_IMOVEL .OR. PARTE <> ePARTE
            @ LIN,40  SAY "SubTotal    : "
            @ LIN,94  SAY REPLICATE("-",20)
            LIN = LIN + 1
            @ LIN,94  SAY eSUB_PARC PICT "@R 999,999,999,999.99" 
            eTOT_PARC = eTOT_PARC + eSUB_PARC
            vDEVEDOR[IIF(eTOT_NPAR >= 1 .AND. eTOT_NPAR <= 10,eTOT_NPAR,11)] = vDEVEDOR[IIF(eTOT_NPAR >= 1 .AND. eTOT_NPAR <= 10,eTOT_NPAR,11)] + 1
            vVLR_DEVE[IIF(eTOT_NPAR >= 1 .AND. eTOT_NPAR <= 10,eTOT_NPAR,11)] = vVLR_DEVE[IIF(eTOT_NPAR >= 1 .AND. eTOT_NPAR <= 10,eTOT_NPAR,11)] + eSUB_PARC
            eSUB_PARC = 0
            eTOT_NPAR = 0
            LIN = LIN + 2
         ENDIF
         DO PTECLA
         IF sisINTERR
            IF eSAIDA = "ARQUIVO"
               SET PRINT OFF
               SET DEVICE TO SCREEN
            ELSE
               SET DEVICE TO SCREEN
            ENDIF
            CLOSE ALL
            RETURN nil
         ENDIF
      ENDDO
      IF EOF()
         @ 56,00 SAY REPLICATE("=",135)
         @ 57,104 SAY sisTIT3
         @ 01,00 SAY " "+sisLETRAN
         @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
         @ 01,120 SAY "Data : "+dtoc(DATE())
         @ 02,00  SAY "Sistema : "+sisTIT2
         @ 02,121 SAY "Relat: SAN004"
         @ 03,30  SAY sisLETRAE+"RELATORIO INADIMPLENTES ATE "+DTOC(eDATA_FIM)+sisLETRAC
         @ 04,00  SAY "    Produto   :"+ALLTRIM(eDESC_PRO)+IIF(eQUADRA = SPACE(5)," ",+" - "+ALLTRIM(eDESC_PRO)+" - Quadra\Bloco : "+ALLTRIM(eQUADRA))
         @ 05,00  SAY REPLICATE("=",135)
         @ 06,02  SAY "No. IMOVEL"
         @ 06,15  SAY "PROPRIETARIO"
         @ 06,65  SAY "DATA VENC."
         @ 06,77  SAY "TIPO"
         @ 06,85  SAY "NUM.PARC."
         @ 06,94  SAY "VLR. PARCELA"
         @ 06,115 SAY "OBS"
         @ 07,00 SAY REPLICATE("=",135)
         LIN = 13
         eTOT_CASO = 0
         eAUX = 1
         DO WHILE eAUX <= 11
            IF eAUX <= 10
               @ LIN,30   SAY "DEVEDORES COM "+STR(eAUX,2)+" PARCELAS : "
               @ LIN,86   SAY vDEVEDOR[eAUX]    PICT "@R 9,999"
               @ LIN,94   SAY vVLR_DEVE[eAUX]   PICT "@R 999,999,999,999.99"
            ELSE
               @ LIN,30   SAY "DEVEDORES COM MAIS DE 10 PARCELAS : "
               @ LIN,86   SAY vDEVEDOR[eAUX]    PICT "@R 9,999"
               @ LIN,94   SAY vVLR_DEVE[eAUX]   PICT "@R 999,999,999,999.99"
            ENDIF
            LIN = LIN + 1
            eTOT_CASO = eTOT_CASO + vDEVEDOR[eAUX]
            eAUX = eAUX + 1
         ENDDO
         LIN = LIN + 3
         @ LIN,30   SAY "TOTAL GERAL : "
         @ LIN,86   SAY eTOT_CASO   PICT "@R 9,999"                                                               
         @ LIN,94   SAY eTOT_PARC   PICT "@R 999,999,999,999.99"
      ENDIF
      @ 56,00 SAY REPLICATE("=",135)
      @ 57,104 SAY sisTIT3
      @ 01,00 SAY " "+sisLETRAN
   ENDDO
ENDIF
IF eSAIDA = "ARQUIVO"
   SET PRINT TO
   SET PRINT OFF
   SET DEVICE TO SCREEN
ELSE
   SET PRINT TO
   SET DEVICE TO SCREEN
ENDIF
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
