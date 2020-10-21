*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_DEVE.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE DEVEDORES NO PERIODO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function REL_DEVE()
SAVE SCREEN TO TELA03
PTIT( "RELAT‡RIO DE DEVEDORES")
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
   DBEDIT(5,15,5,70,CAMPO,"FUNC_CONS",PIC,CAB," "," - "," ")
   eCLAS_PRO = CODIGO
   eTIPO_PRO = TIPO
   eDESC_PRO = DESCRICAO
   EXIT
ENDDO
* Selecao da rotina de Alteracao conforme o tipo de produto
eQUADRA  = SPACE(1)
DO CASE
   CASE eTIPO_PRO = 1
      * Consulta de Loteamentos
      SELE 3
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA     = SPACE(5)
      eNUM_IMOVEL = SPACE(5)
      PLQUA("01")
      PPERG ( "Deseja Selecionar "+ALLTRIM(sisGRUPO1)+" (S/N) ? ")
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
      PPERG ( "Deseja Selecionar "+ALLTRIM(sisGRUPO2)+" (S/N) ? ")
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
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"DEVEDORES.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "DEVEDORES.TXT"+SPACE(10) 
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

eDATA_INI   = CTOD("")
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
PLQUA("01")
eORIGEM = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Origem dos Pagamentos (C)arteira,(B)anco ou (A)mbas ? " GET eORIGEM PICT "@!X" VALID eORIGEM $("CBA")
READ
eTIPO = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Tipo de Parcelas (S)inal,(N)ormais,(I)ntermediarias ou (A)mbas ? " GET eTIPO PICT "@!X" VALID eTIPO $("SNIA")
READ
ePROTESTO = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Selecionar T°tulos † (P)rotesto,(N)∆o Protesto ou (A)mbos ? " GET ePROTESTO PICT "@!X" VALID ePROTESTO $("PNA")
READ
eDEVEDOR = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Selecionar Todos os (D)evedores ou descontar os (P)agos ? " GET eDEVEDOR PICT "@!X" VALID eDEVEDOR $("DP")
READ
PLQUA("01")
eORDEM = SPACE(1)
@ 23,00 SAY "Emiss∆o de Relat¢rio por Ordem de Vencimento ou "+ALLTRIM(sisUNID1)+" (V/I) ? " GET eORDEM PICT "@!X" VALID eORDEM $("VI")
READ
eTOTALI = .F.
IF eORDEM = "I"
   eTOTALI = .F.
   PLQUA("01") 
   PPERG ( "Deseja Total por "+ALLTRIM(sisUNID1)+" (S/N) ? ")
   IF sisRESP = "S"
      eTOTALI = .T.
   ELSE
      eTOTALI = .F.
   ENDIF
ELSE
   eTOTALI = .F.
ENDIF
eINADIMP = .F.
PLQUA("01") 
PPERG ( "Deseja Considerar a Selecao de Inadimplencia (S/N) ? ")
IF sisRESP = "S"
   eINADIMP = .T.
ELSE
   eINADIMP = .F.
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
   USE &eARQ INDEX &eIND2,&eIND3,&eIND1
   GOTO TOP
   IF eORDEM = "V"
      USE &eARQ INDEX &eIND2,&eIND3,&eIND1
      GOTO TOP
   ELSE
      USE &eARQ INDEX &eIND1,&eIND2,&eIND3
      GOTO TOP
   ENDIF
   IF eDEVEDOR = "P"
      * DEVEDORES MENOS OS PAGOS
      DO CASE
         CASE eTIPO = "A"
            IF eORIGEM = "A"
               DO CASE
                  CASE ePROTESTO = "A"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL
                  CASE ePROTESTO = "P"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO
                  CASE ePROTESTO = "N"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO          
               ENDCASE
            ELSE 
               IF eORIGEM = "C"
                  DO CASE
                     CASE ePROTESTO = "A"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B"
                     CASE ePROTESTO = "P"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO
                     CASE ePROTESTO = "N"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO          
                  ENDCASE
               ELSE
                  DO CASE
                     CASE ePROTESTO = "A"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "
                     CASE ePROTESTO = "P"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO
                     CASE ePROTESTO = "N"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO          
                  ENDCASE
               ENDIF
            ENDIF
         CASE eTIPO = "N"
            IF eORIGEM = "A"
               DO CASE
                  CASE ePROTESTO = "A"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL ;
                     .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                  CASE ePROTESTO = "P"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                     .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                  CASE ePROTESTO = "N"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                     .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
               ENDCASE
            ELSE 
               IF eORIGEM = "C"
                  DO CASE
                     CASE ePROTESTO = "A"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                     CASE ePROTESTO = "P"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                     CASE ePROTESTO = "N"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                  ENDCASE
               ELSE
                  DO CASE
                     CASE ePROTESTO = "A"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                     CASE ePROTESTO = "P"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                     CASE ePROTESTO = "N"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                  ENDCASE
               ENDIF
            ENDIF
         CASE eTIPO = "I" 
            IF eORIGEM = "A"
               DO CASE
                  CASE ePROTESTO = "A"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL ;
                     .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                  CASE ePROTESTO = "P"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                     .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                  CASE ePROTESTO = "N"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                     .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
               ENDCASE
            ELSE 
               IF eORIGEM = "C"
                  DO CASE
                     CASE ePROTESTO = "A"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                     CASE ePROTESTO = "P"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                     CASE ePROTESTO = "N"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                  ENDCASE
               ELSE
                  DO CASE
                     CASE ePROTESTO = "A"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                     CASE ePROTESTO = "P"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                     CASE ePROTESTO = "N"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                  ENDCASE
               ENDIF
            ENDIF
         CASE eTIPO = "S" 
            IF eORIGEM = "A"
               DO CASE
                  CASE ePROTESTO = "A"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL ;
                     .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                  CASE ePROTESTO = "P"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                     .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                  CASE ePROTESTO = "N"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                     .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
               ENDCASE
            ELSE 
               IF eORIGEM = "C"
                  DO CASE
                     CASE ePROTESTO = "A"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                     CASE ePROTESTO = "P"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                     CASE ePROTESTO = "N"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                  ENDCASE
               ELSE
                  DO CASE
                     CASE ePROTESTO = "A"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                     CASE ePROTESTO = "P"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                     CASE ePROTESTO = "N"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                  ENDCASE
               ENDIF
            ENDIF
      ENDCASE
   ELSE
      * DEVEDORES MAIS OS PAGOS
      DO CASE
         CASE eTIPO = "A"
            IF eORIGEM = "A"
               DO CASE
                  CASE ePROTESTO = "A"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL
                  CASE ePROTESTO = "P"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO
                  CASE ePROTESTO = "N"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO          
               ENDCASE
            ELSE 
               IF eORIGEM = "C"
                  DO CASE
                     CASE ePROTESTO = "A"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B"
                     CASE ePROTESTO = "P"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO
                     CASE ePROTESTO = "N"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO          
                  ENDCASE
               ELSE
                  DO CASE
                     CASE ePROTESTO = "A"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "
                     CASE ePROTESTO = "P"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO
                     CASE ePROTESTO = "N"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO          
                  ENDCASE
               ENDIF
            ENDIF
         CASE eTIPO = "N"
            IF eORIGEM = "A"
               DO CASE
                  CASE ePROTESTO = "A"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL ;
                     .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                  CASE ePROTESTO = "P"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                     .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                  CASE ePROTESTO = "N"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                     .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
               ENDCASE
            ELSE 
               IF eORIGEM = "C"
                  DO CASE
                     CASE ePROTESTO = "A"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                     CASE ePROTESTO = "P"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                     CASE ePROTESTO = "N"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                  ENDCASE
               ELSE
                  DO CASE
                     CASE ePROTESTO = "A"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                     CASE ePROTESTO = "P"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                     CASE ePROTESTO = "N"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                  ENDCASE
               ENDIF
            ENDIF
         CASE eTIPO = "I" 
            IF eORIGEM = "A"
               DO CASE
                  CASE ePROTESTO = "A"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL ;
                     .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                  CASE ePROTESTO = "P"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                     .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                  CASE ePROTESTO = "N"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                     .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
               ENDCASE
            ELSE 
               IF eORIGEM = "C"
                  DO CASE
                     CASE ePROTESTO = "A"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                     CASE ePROTESTO = "P"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                     CASE ePROTESTO = "N"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                  ENDCASE
               ELSE
                  DO CASE
                     CASE ePROTESTO = "A"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                     CASE ePROTESTO = "P"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                     CASE ePROTESTO = "N"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                  ENDCASE
               ENDIF
            ENDIF
         CASE eTIPO = "S" 
            IF eORIGEM = "A"
               DO CASE
                  CASE ePROTESTO = "A"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL ;
                     .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                  CASE ePROTESTO = "P"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                     .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                  CASE ePROTESTO = "N"
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                     .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
               ENDCASE
            ELSE 
               IF eORIGEM = "C"
                  DO CASE
                     CASE ePROTESTO = "A"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                     CASE ePROTESTO = "P"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                     CASE ePROTESTO = "N"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                  ENDCASE
               ELSE
                  DO CASE
                     CASE ePROTESTO = "A"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                     CASE ePROTESTO = "P"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                     CASE ePROTESTO = "N"
                        SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                        .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                  ENDCASE
               ENDIF
            ENDIF
      ENDCASE
   ENDIF
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
         DO PLQUA   WITH "01"
		 PMENSAG("  Acumulando  Parcelas  do  Produto  :  "+ALLTRIM(eIND_CLAS)+" Quadra\Bloco : "+ALLTRIM(eIND_QUAD))
         SELE 8
         USE TEMP_PAR
         GOTO TOP
         eZERO = "000"
         eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)

         SELDEVAP()

         GOTO TOP
         SELE 3
         SKIP
      ENDDO
      SET EXCLUSIVE ON
      SELE 8
      USE TEMP_PAR
      IF eORDEM = "V"
         INDEX ON DATA_VENC TO TEMP_PAR
         GOTO TOP
         USE TEMP_PAR INDEX TEMP_PAR
         GOTO TOP
      ELSE
         INDEX ON CLASSE+QUADRA+NUM_IMOVEL+STR(PARTE,1) TO TEMP_PAR
         GOTO TOP
         USE TEMP_PAR INDEX TEMP_PAR
         GOTO TOP
      ENDIF
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
         DO PLQUA   WITH "01"
		 PMENSAG("  Acumulando  Parcelas  do  Produto  :  "+eIND_CLAS+" Quadra\Bloco : "+eIND_QUAD)
         SELE 8
         USE TEMP_PAR
         GOTO TOP
         eZERO = "000"
         eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)

         SELDEVAP()

         GOTO TOP
         SELE 4
         SKIP
      ENDDO
      SET EXCLUSIVE ON
      SELE 8
      USE TEMP_PAR
      IF eORDEM = "V"
         INDEX ON DATA_VENC TO TEMP_PAR
         GOTO TOP
         USE TEMP_PAR INDEX TEMP_PAR
         GOTO TOP
      ELSE
         INDEX ON CLASSE+QUADRA+NUM_IMOVEL+STR(PARTE,1) TO TEMP_PAR
         GOTO TOP
         USE TEMP_PAR INDEX TEMP_PAR
         GOTO TOP
      ENDIF
      SET EXCLUSIVE OFF
   ENDIF
ENDIF
IF eINADIMP
   PLQUA("01")
   PMENSAG("Zerando Inadimplencia, Aguarde !")
   SELE 8
   GOTO TOP
   DO WHILE .NOT. EOF()
      eCLASSE     = CLASSE
      eQUADRA     = QUADRA
      eNUM_IMOVEL = NUM_IMOVEL
      ePARTE      = PARTE
      DO PLQUA   WITH "01"
	  PMENSAG("Zerando Parcelas do Produto : "+eCLASSE+" Quadra\Bloco : "+eQUADRA)
      SELE 6
      GOTO TOP
      SEEK eCLASSE+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
      eNUM_CLIE = 0
      eNUM_CLIE = NUM_CLIE
      SELE 5
      GOTO TOP
      SEEK eNUM_CLIE
      IF SELECAO = " "
         SELE 8
         IF F_BLOQR()
            DELETE
            UNLOCK
         ENDIF
      ENDIF
      SELE 8
      SKIP
   ENDDO
ENDIF
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
   eCLASSE     = CLASSE
   eQUADRA     = QUADRA
   eNUM_IMOVEL = NUM_IMOVEL
   ePARTE      = PARTE
   eDATA_PGTO = DATA_PGTO
   eTOTAL     = 0
   eMES       = MONTH(DATA_VENC)
   eSUB_TIT   = 0
   eSUB_PARC  = 0
   eTOT_TIT   = 0
   eTOT_PARC  = 0
   DO WHILE .NOT. EOF()
      @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
      @ 01,115 SAY "Data : "+dtoc(DATE())
      @ 02,00  SAY "Sistema : "+sisTIT2
      @ 02,115 SAY "Relat: SAN004"
      IF eDEVEDOR = "P"
         @ 03,20  SAY sisLETRAE+"RELATORIO DEVEDORES DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
      ELSE
         @ 03,20  SAY sisLETRAE+"RELATORIO DEVEDORES TOTAIS DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
      ENDIF
      @ 04,00  SAY "   Produto     : "+STR(eCLAS_PRO,5)+" - "+ALLTRIM(eDESC_PRO)+IIF(eQUADRA = SPACE(5)," ",+" - Quadra\Bloco : "+ALLTRIM(eQUADRA))
      @ 05,00  SAY REPLICATE("=",135)
      @ 06,02  SAY "No. IMOVEL"
      @ 06,15  SAY "DATA VENC."
      @ 06,27  SAY "TIPO"
      @ 06,35  SAY "NUM.PARC."
      @ 06,47  SAY "VLR. PARCELA"
      @ 06,70  SAY "COD. DOCUM."
      @ 06,90  SAY "PROTESTO"
      @ 06,105 SAY "CART./BANC."
      @ 06,115 SAY "OBS"
      @ 07,00 SAY REPLICATE("=",135)
      LIN = 8
      DO WHILE .NOT. EOF() .AND. LIN <= 54
         @ LIN,02  SAY STR(PARTE,1)+"/"+ALLTRIM(QUADRA)+"/"+ALLTRIM(NUM_IMOVEL)
         eCLASSE     = CLASSE
         eQUADRA     = QUADRA
         eNUM_IMOVEL = NUM_IMOVEL
         ePARTE      = PARTE
         SELE 6
         GOTO TOP
         SEEK eCLASSE+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
         eNUM_PARC = 0
         eNUM_PARC = PARCELAS
         SELE 8
         @ LIN,15  SAY DATA_VENC
         @ LIN,29  SAY TIPO_PARC
         @ LIN,36  SAY ALLTRIM(NUM_PARC)+"/"+ALLTRIM(STR(eNUM_PARC,5))
         @ LIN,44  SAY VLR_PARC   PICT "@R 999,999,999,999.99"
         @ LIN,70  SAY COD_BANCO
         @ LIN,90  SAY IIF(PROTESTO," SIM "," NAO ")
         @ LIN,105 SAY IIF(ORIGEM = "B","CARTEIRA","BANCARIA")
         @ LIN,115 SAY OBS
         eSUB_PARC  = eSUB_PARC + VLR_PARC
         eSUB_TIT   = eSUB_TIT + 1
         LIN = LIN + 1
         SKIP
         IF eTOTALI
            IF eQUADRA = QUADRA .AND. eNUM_IMOVEL = NUM_IMOVEL .AND. ePARTE = PARTE


            ELSE
               @ LIN,02  SAY "SubTotal ("+ALLTRIM(eCLASSE)+"/"+ALLTRIM(eQUADRA)+"/"+ALLTRIM(eNUM_IMOVEL)+"/"+ALLTRIM(STR(ePARTE,1))+")   : "
               @ LIN,31  SAY eSUB_TIT    PICT "@R 999,999"
               @ LIN,44  SAY eSUB_PARC   PICT "@R 999,999,999,999.99"
               LIN = LIN + 2
               eTOT_PARC   = eTOT_PARC  + eSUB_PARC
               eTOT_TIT    = eTOT_TIT   + eSUB_TIT
               eSUB_PARC   = 0
               eSUB_TIT    = 0
               eCLASSE     = CLASSE
               eQUADRA     = QUADRA
               eNUM_IMOVEL = NUM_IMOVEL
               ePARTE      = PARTE
            ENDIF
         ELSE
            eTOT_PARC  = eTOT_PARC  + eSUB_PARC
            eTOT_TIT   = eTOT_TIT   + eSUB_TIT
            eSUB_PARC  = 0
            eSUB_TIT   = 0
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
         LIN = LIN + 1
         @ LIN,02  SAY "Total Geral    : "
         @ LIN,31  SAY eTOT_TIT    PICT "@R 999,999"
         @ LIN,44  SAY eTOT_PARC   PICT "@R 999,999,999,999.99"
         LIN = LIN + 2
         IF eDEVEDOR = "P"
            @ LIN,02  SAY "OBSERVACAO : RELATORIO DEVEDORES EXCLUINDO AS PARCELAS PAGAS"
         ELSE
            @ LIN,02  SAY "OBSERVACAO : RELATORIO DEVEDORES TOTAIS INCLUINDO AS PARCELAS PAGAS"
         ENDIF
      ENDIF
      @ 56,00 SAY REPLICATE("=",135)
      @ 57,95 SAY sisTIT3
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
************************* funcao ********************************
FUNCTION SELDEVAP
         IF eDEVEDOR = "P"
            * DEVEDORES MENOS OS PAGOS
            DO CASE
               CASE eTIPO = "A"
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO          
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B"
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO          
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO          
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "N"
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "I" 
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "S" 
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                        ENDCASE
                     ENDIF
                  ENDIF
            ENDCASE
         ELSE
            * DEVEDORES MAIS OS PAGOS
            DO CASE
               CASE eTIPO = "A"
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO          
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B"
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO          
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO          
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "N"
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "I" 
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "S" 
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM ) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
                        ENDCASE
                     ENDIF
                  ENDIF
            ENDCASE
         ENDIF


RETURN .T.
*************************  fim  *********************************
