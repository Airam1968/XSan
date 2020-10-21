*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_DEVE.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE DEVEDORES NO PERIODO CORRIGIDO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function REL_DEV2()
SAVE SCREEN TO TELA03
PTIT( "RELATÓRIO DE DEVEDORES CORRIGIDO")
SELE 1
USE STATUS
eANATOCISMO = ANATOCISMO
eCPF_CGC    = CGC_CPF
eTX_MULTA   = MULTA/100
eTX_DESCON  = DESCONTO/100
eCIDADE     = CIDADE
eCORR_ATRA  = CORR_ATRA
ePRO_RATA   = PRO_RATA
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
SELE 7
USE TAXA_MES INDEX TAXA_MES
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
      PMSGR( "Datas Inv lidas !",3)
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

eDATA_COR   = DATE()
DO WHILE .T.
   @ 10,02 SAY "Correção Até : " GET eDATA_COR
   READ
   IF eDATA_COR = CTOD("")
      PLQUA("01")
      PMSGR( "Data Inválida !",3)
      LOOP
   ELSE
      EXIT
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
@ 23,00 SAY "Selecionar Títulos (P)rotesto,(N)ão Protesto ou (A)mbos ? " GET ePROTESTO PICT "@!X" VALID ePROTESTO $("PNA")
READ
eDEVEDOR = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Selecionar Todos os (D)evedores ou descontar os (P)agos ? " GET eDEVEDOR PICT "@!X" VALID eDEVEDOR $("DP")
READ
PLQUA("01")
eORDEM = SPACE(1)
@ 23,00 SAY "EmissÆo de Relat¢rio por Ordem de Vencimento ou "+ALLTRIM(sisUNID1)+" (V/I) ? " GET eORDEM PICT "@!X" VALID eORDEM $("VI")
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
*PLQUA("01") 
*PPERG ( "Deseja Considerar a Selecao de Inadimplencia (S/N) ? ")
*IF sisRESP = "S"
*   eINADIMP = .T.
*ELSE
*   eINADIMP = .F.
*ENDIF
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
   PMSGR( "Aten‡Æo !",3)
   PMENSAG("Aguarde a montagem do Banco de Dados para Impressão ...")
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

         SELDEVA2()

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

         SELDEVA2()

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
   PMSGR( "Aten‡Æo !",3)
   PMENSAG("Não Há Devedores Neste Período !")
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
   eSUB_CORR  = 0   
   eTOT_TIT   = 0
   eTOT_PARC  = 0
   eTOT_CORR  = 0   
   DO WHILE .NOT. EOF()
      @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
      @ 01,115 SAY "Data : "+dtoc(DATE())
      @ 02,00  SAY "Sistema : "+sisTIT2
      @ 02,115 SAY "Relat: SAN004"
      IF eDEVEDOR = "P"
         @ 03,20  SAY sisLETRAE+"RELATORIO DEVEDORES DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+" CORRIGIDO ATÉ "+DTOC(eDATA_COR)+sisLETRAC
      ELSE
         @ 03,20  SAY sisLETRAE+"RELATORIO DEVEDORES TOTAIS DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+" CORRIGIDO ATÉ "+DTOC(eDATA_COR)+sisLETRAC
      ENDIF
      @ 04,00  SAY "   Produto     : "+STR(eCLAS_PRO,5)+" - "+ALLTRIM(eDESC_PRO)+IIF(eQUADRA = SPACE(5)," ",+" - Quadra\Bloco : "+ALLTRIM(eQUADRA))
      @ 05,00  SAY REPLICATE("=",135)
      @ 06,02  SAY "No. IMOVEL"
      @ 06,15  SAY "DATA VENC."
      @ 06,27  SAY "TIPO"
      @ 06,35  SAY "NUM.PARC."
      @ 06,47  SAY "VLR. PARCELA"
      @ 06,70  SAY "COD. DOCUM."
      @ 06,90  SAY "VLR. CORRIGIDO"
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
		eINDICE   = INDICE		 
		SELE 8
		@ LIN,15  SAY DATA_VENC
		@ LIN,29  SAY TIPO_PARC
		@ LIN,36  SAY ALLTRIM(NUM_PARC)+"/"+ALLTRIM(STR(eNUM_PARC,5))
		@ LIN,44  SAY VLR_PARC   PICT "@R 999,999,999,999.99"
		@ LIN,70  SAY COD_BANCO

		*********
		** Calculo da Correção
		eAUX_ACUM  = 0
		ePERID_COMI= PERID_COMI
		eTX_MULTA  = MULTA
		eTX_COMPER = COMISSAO
		eVLR_PARC  = VLR_PARC
		eDATA_VENC = DATA_VENC
		eVLR_PAGO  = 0
		eDATA_PAGO = eDATA_COR
		eMULTA     = 0
		eJUROS     = 0
		eDESCONTO  = 0
		eAUX       = 0
		eOBS2      = ""		
		IF sisCGC_CPF <> '00915210000100'
			*Calculo Normal
			DO CASE
			   CASE eDATA_VENC = eDATA_PAGO
				  * "Pagamento dentro do Prazo, Aguarde ... 1"
				  eDESCONTO = 0
				  eMULTA    = 0
				  eJUROS    = 0
				  eCORRECAO = 0
			   CASE eDATA_VENC > eDATA_PAGO
				  * Calculo de Desconto de Parcelas
				  eDIA_VENC = 0
				  eMES_VENC = 0
				  eANO_VENC = 0
				  eDIA_VENC = SUBSTR(DTOC(eDATA_VENC),1,2)
				  eMES_VENC = SUBSTR(DTOC(eDATA_VENC),4,2)
				  eANO_VENC = SUBSTR(DTOC(eDATA_VENC),7,4)
				  eDIA_VENC = VAL(eDIA_VENC)
				  eMES_VENC = VAL(eMES_VENC) - 1
				  eANO_VENC = VAL(eANO_VENC) 
				  IF eMES_VENC = 0
					 eMES_VENC = 12
					 eANO_VENC = eANO_VENC - 1
				  ENDIF
				  eDIA_VECC = eDIA_VENC
				  eMES_VECC = eMES_VENC
				  eANO_VECC = eANO_VENC
				  eTAM_DIA  = LENNUM(INT(eDIA_VENC))
				  eTAM_MES  = LENNUM(INT(eMES_VENC))
				  eDIA_VENC = IIF(eTAM_DIA = 1,"0"+STR(eDIA_VENC,1),STR(eDIA_VENC,2))
				  eMES_VENC = IIF(eTAM_MES = 1,"0"+STR(eMES_VENC,1),STR(eMES_VENC,2))
				  eANO_VENC = STR(INT(eANO_VENC),4)
				  eMES_ANT  = CTOD(eDIA_VENC+"/"+eMES_VENC+"/"+eANO_VENC)
				  DO CASE
					 CASE eDATA_VENC > eDATA_PAGO .AND. eDATA_PAGO < eMES_ANT
					 DO PLQUA WITH "01"
					 DO PMENSAG WITH "Calculo de Desconto, Aguarde ..."
					 eMULTA    = 0
					 eJUROS    = 0
					 eCORRECAO = 0
					 eAUX      = 1
					 eDIA_VENC = 0
					 eMES_VENC = 0
					 eANO_VENC = 0
					 eDIA_VENC = SUBSTR(DTOC(eDATA_VENC),1,2)
					 eMES_VENC = SUBSTR(DTOC(eDATA_VENC),4,2)
					 eANO_VENC = SUBSTR(DTOC(eDATA_VENC),7,4)
					 eDIA_PAGO = 0
					 eMES_PAGO = 0
					 eANO_PAGO = 0
					 eDIA_PAGO = SUBSTR(DTOC(eDATA_PAGO),1,2)
					 eMES_PAGO = SUBSTR(DTOC(eDATA_PAGO),4,2)
					 eANO_PAGO = SUBSTR(DTOC(eDATA_PAGO),7,4)
					 eAUX_VENC = CTOD("")
					 eDIA_VENC = VAL(eDIA_VENC)
					 eMES_VENC = VAL(eMES_VENC)
					 eANO_VENC = VAL(eANO_VENC)
					 DO WHILE .T.
						eMES_VENC = eMES_VENC - 1
						IF eMES_VENC = 0
						   eMES_VENC = 12
						   eANO_VENC = eANO_VENC - 1
						ENDIF
						eAUX      = eAUX + 1
						eTAM_DIA  = LENNUM(INT(eDIA_VENC))
						eTAM_MES  = LENNUM(INT(eMES_VENC))
						****
						* Tratamento para o Dia de Vencimento Fevereiro
						IF (eDIA_VENC > 28) .AND. (eMES_VENC = 2)
						eTEMP_VENC = CTOD("01/03/"+STR(INT(eANO_VENC),4))-1
						eDIA_VENC = VAL(SUBSTR(DTOC(eTEMP_VENC),1,2))
						ENDIF
						**** C¢digo Original            
						*eDIA_VENC = IIF(eDIA_VENC > 28 .AND. eMES_VENC = 2,28,eDIA_VENC)
						
						eDIA_VENC = IIF(eTAM_DIA = 1,"0"+STR(eDIA_VENC,1),STR(eDIA_VENC,2))
						eMES_VENC = IIF(eTAM_MES = 1,"0"+STR(eMES_VENC,1),STR(eMES_VENC,2))
						eANO_VENC = STR(INT(eANO_VENC),4)
						eAUX_VENC = CTOD(eDIA_VENC+"/"+eMES_VENC+"/"+eANO_VENC)
						eDIA_VENC = VAL(IIF(eTAM_DIA = 1,"0"+ALLTRIM(eDIA_VENC),ALLTRIM(eDIA_VENC)))
						eMES_VENC = VAL(IIF(eTAM_MES = 1,"0"+ALLTRIM(eMES_VENC),ALLTRIM(eMES_VENC)))
						eANO_VENC = VAL(eANO_VENC)
						IF MONTH(eAUX_VENC) = MONTH(eDATA_PAGO) .AND. YEAR(eAUX_VENC) = YEAR(eDATA_PAGO)
						   EXIT
						ENDIF
					 ENDDO
					 IF eAUX_VENC < eDATA_PAGO
						eAUX = eAUX - 1
					 ENDIF
					 * Calculo do desconto
					 eDESCONTO = (eVLR_PARC * (eTX_DESCON * eAUX))
					 eMULTA    = 0
					 eJUROS    = 0
					 eCORRECAO = 0
				  OTHERWISE
					 eDESCONTO = 0
					 eMULTA    = 0
					 eJUROS    = 0
					 eCORRECAO = 0
				  ENDCASE
			   OTHERWISE
				  * Calculo de Multa e Juros
				  DO CASE
				  CASE eDATA_PAGO > eDATA_VENC  .AND. ((eDATA_PAGO - eDATA_VENC) >= 30)
					 * "Calculo de Multa e Juros, Aguarde ...3"
					 eDESCONTO = 0
					 eMULTA    = 0
					 eJUROS    = 0
					 eCORRECAO = 0
					 eAUX      = 1
					 eDIA_VENC = 0
					 eMES_VENC = 0
					 eANO_VENC = 0
					 eDIA_VENC = SUBSTR(DTOC(eDATA_VENC),1,2)
					 eMES_VENC = SUBSTR(DTOC(eDATA_VENC),4,2)
					 eANO_VENC = SUBSTR(DTOC(eDATA_VENC),7,4)
					 eDIA_VENO = eDIA_VENC
					 eMES_VENO = eMES_VENC
					 eANO_VENO = eANO_VENC
					 eDIA_PAGO = 0
					 eMES_PAGO = 0
					 eANO_PAGO = 0
					 eDIA_PAGO = SUBSTR(DTOC(eDATA_PAGO),1,2)
					 eMES_PAGO = SUBSTR(DTOC(eDATA_PAGO),4,2)
					 eANO_PAGO = SUBSTR(DTOC(eDATA_PAGO),7,4)
					 eAUX_VENC = CTOD("")
					 eDIA_VECC = VAL(eDIA_VENC)
					 eMES_VECC = VAL(eMES_VENC)
					 eANO_VECC = VAL(eANO_VENC)
					 eMES_VECC = eMES_VECC + 1
					 eDIF_DIAS = 30 - VAL(eDIA_PAGO)
					 IF eMES_VECC = 13
						eMES_VECC = 01
						eANO_VECC = eANO_VECC + 1
					 ENDIF
					 eTAM_MES  = LENNUM(INT(eMES_VECC))
					 eMES_VECC = IIF(eTAM_MES = 1,"0"+STR(eMES_VECC,1),STR(eMES_VECC,2))
					 eANO_VECC = STR(INT(eANO_VECC),4)
					 DO WHILE .T.
						eDIA_VENC = VAL(eDIA_VENC)
						eMES_VENC = VAL(eMES_VENC)
						eANO_VENC = VAL(eANO_VENC)
						eMES_VENC = eMES_VENC + 1
						IF eMES_VENC = 13
						   eMES_VENC = 01
						   eANO_VENC = eANO_VENC + 1
						ENDIF
						eAUX = eAUX + 1
						eTAM_DIA  = LENNUM(INT(eDIA_VENC))
						eTAM_MES  = LENNUM(INT(eMES_VENC))
						****
						* Tratamento para o Dia de Vencimento Fevereiro
						IF (eDIA_VENC > 28) .AND. (eMES_VENC = 2)
						eTEMP_VENC = CTOD("01/03/"+STR(INT(eANO_VENC),4))-1
						eDIA_VENC = VAL(SUBSTR(DTOC(eTEMP_VENC),1,2))
						ENDIF
						**** C¢digo Original            
						*eDIA_VENC = IIF(eDIA_VENC > 28 .AND. eMES_VENC = 2,28,eDIA_VENC)
						
						eDIA_VENC = IIF(eTAM_DIA = 1,"0"+STR(eDIA_VENC,1),STR(eDIA_VENC,2))
						eMES_VENC = IIF(eTAM_MES = 1,"0"+STR(eMES_VENC,1),STR(eMES_VENC,2))
						eANO_VENC = STR(INT(eANO_VENC),4)
						eAUX_VENC = CTOD(eDIA_VENC+"/"+eMES_VENC+"/"+eANO_VENC)
						IF (MONTH(eAUX_VENC) = MONTH(eDATA_PAGO)) .AND. (YEAR(eAUX_VENC) = YEAR(eDATA_PAGO))
						   EXIT
						ENDIF
					 ENDDO
					 IF eAUX_VENC > eDATA_PAGO
						eAUX = eAUX - 1
					 ENDIF
					 IF eDIF_DIAS > 0
						SELE 7
						GOTO TOP
						SET FILTER TO (ANO+MES = eANO_VENO+eMES_VENO)
						GOTO TOP
					 IF (eINDICE <> "NENHUM")
					   eIND_ACUM = ((&eINDICE/100) + 1)
					   eCORRECAO = eCORRECAO + (eVLR_PARC * ((eIND_ACUM ^ (( 1000 / 30 ) / 1000) ^ eDIF_DIAS) - 1))
					 ELSE     
					   eIND_ACUM = 0
					   eCORRECAO = 0
					 ENDIF                
					 ENDIF
					 SELE 7
					 GOTO TOP
					 SET FILTER TO ((ANO+MES >= eANO_VECC+eMES_VECC) .AND. (ANO+MES <= eANO_PAGO+eMES_PAGO))
					 GOTO TOP
					 eIND_ACUM  = 1
					 DO WHILE (ANO+MES <= eANO_PAGO+eMES_PAGO) .AND. .NOT. EOF()
						IF (eINDICE <> "NENHUM")
							   eIND_ACUM = ((&eINDICE/100) + 1)
							   eCORRECAO = eCORRECAO + (eVLR_PARC * (eIND_ACUM - 1))
						ELSE     
							   eIND_ACUM = 0
							   eCORRECAO = 0
						ENDIF            
						SKIP
					 ENDDO
					 IF ePERID_COMI = "a.m."
						eJUROS    = (eVLR_PARC + eCORRECAO) * (((eTX_COMPER + 1) ^ (eAUX - 1)) - 1)
					 ELSE
						eJUROS    = (eVLR_PARC + eCORRECAO) * (((eTX_COMPER + 1) ^ (eDATA_PAGO - eDATA_VENC)) - 1)
					 ENDIF
					 * Calculo do Multa e Juros
					 eMULTA    = (eVLR_PARC + eCORRECAO) * eTX_MULTA
					 eDESCONTO = 0
				  CASE eDATA_PAGO > eDATA_VENC .AND. ((eDATA_PAGO - eDATA_VENC) < 30)
					 * "Calculo de Multa, Aguarde ..."
					 eDESCONTO = 0
					 eMULTA    = 0
					 eJUROS    = 0
					 eCORRECAO = 0
					 eAUX      = 1
					 eDIA_VENC = 0
					 eMES_VENC = 0
					 eANO_VENC = 0
					 eDIA_VENC = SUBSTR(DTOC(eDATA_VENC),1,2)
					 eMES_VENC = SUBSTR(DTOC(eDATA_VENC),4,2)
					 eANO_VENC = SUBSTR(DTOC(eDATA_VENC),7,4)
					 eDIA_VENO = eDIA_VENC
					 eMES_VENO = eMES_VENC
					 eANO_VENO = eANO_VENC
					 eDIA_PAGO = 0
					 eMES_PAGO = 0
					 eANO_PAGO = 0
					 eDIA_PAGO = SUBSTR(DTOC(eDATA_PAGO),1,2)
					 eMES_PAGO = SUBSTR(DTOC(eDATA_PAGO),4,2)
					 eANO_PAGO = SUBSTR(DTOC(eDATA_PAGO),7,4)
					 eAUX_VENC = CTOD("")
					 eDIA_VECC = VAL(eDIA_VENC)
					 eMES_VECC = VAL(eMES_VENC)
					 eANO_VECC = VAL(eANO_VENC)
					 eMES_VECC = eMES_VECC + 1
					 eDIF_DIAS = 30 - VAL(eDIA_PAGO)
					 IF eMES_VECC = 13
						eMES_VECC = 01
						eANO_VECC = eANO_VECC + 1
					 ENDIF
					 eTAM_MES  = LENNUM(INT(eMES_VECC))
					 eMES_VECC = IIF(eTAM_MES = 1,"0"+STR(eMES_VECC,1),STR(eMES_VECC,2))
					 eANO_VECC = STR(INT(eANO_VECC),4)
					 IF eAUX_VENC >= eDATA_PAGO
						eAUX = eAUX - 1
					 ENDIF
					 * Calculo de Correcao
					 IF eDIF_DIAS > 0
						SELE 7
						GOTO TOP
						SET FILTER TO (ANO+MES = eANO_VENO+eMES_VENO)
						GOTO TOP
						IF (eINDICE <> "NENHUM")
							   eIND_ACUM = ((&eINDICE/100) + 1)
							   eCORRECAO = eCORRECAO + (eVLR_PARC * ((eIND_ACUM ^ (( 1000 / 30 ) / 1000) ^ eDIF_DIAS) - 1))
						ELSE     
							   eIND_ACUM = 0
							   eCORRECAO = 0
						ENDIF             
					 ENDIF
					 eDESCONTO = 0
					 IF ePERID_COMI = "a.m."
						eJUROS    = 0
					 ELSE
						eJUROS    = (eVLR_PARC + eCORRECAO) * (((eTX_COMPER + 1) ^ (eDATA_PAGO - eDATA_VENC)) - 1)
					 ENDIF
					 eMULTA    = (eVLR_PARC + eCORRECAO) * eTX_MULTA
			   ENDCASE
			ENDCASE
			*Fim Calculo Normal
		ELSE
			* Calculo Terra SimÆo
			DO CASE
				CASE eDATA_VENC = eDATA_PAGO
				   *Pagamento dentro do Prazo, Aguarde ... 1")
				   eDESCONTO = 0
				   eMULTA    = 0
				   eJUROS    = 0
				   eCORRECAO = 0
				CASE eDATA_VENC > eDATA_PAGO
				   * Calculo de Desconto de Parcelas
				   eDIA_VENC = 0
				   eMES_VENC = 0
				   eANO_VENC = 0
				   eDIA_VENC = SUBSTR(DTOC(eDATA_VENC),1,2)
				   eMES_VENC = SUBSTR(DTOC(eDATA_VENC),4,2)
				   eANO_VENC = SUBSTR(DTOC(eDATA_VENC),7,4)
				   eDIA_VENC = VAL(eDIA_VENC)
				   eMES_VENC = VAL(eMES_VENC) - 1
				   eANO_VENC = VAL(eANO_VENC) 
				   IF eMES_VENC = 0
					  eMES_VENC = 12
					  eANO_VENC = eANO_VENC - 1
				   ENDIF
				   eDIA_VECC = eDIA_VENC
				   eMES_VECC = eMES_VENC
				   eANO_VECC = eANO_VENC
				   eTAM_DIA  = LENNUM(INT(eDIA_VENC))
				   eTAM_MES  = LENNUM(INT(eMES_VENC))
				   eDIA_VENC = IIF(eTAM_DIA = 1,"0"+STR(eDIA_VENC,1),STR(eDIA_VENC,2))
				   eMES_VENC = IIF(eTAM_MES = 1,"0"+STR(eMES_VENC,1),STR(eMES_VENC,2))
				   eANO_VENC = STR(INT(eANO_VENC),4)
				   eMES_ANT  = CTOD(eDIA_VENC+"/"+eMES_VENC+"/"+eANO_VENC)
				   DO CASE
					  CASE eDATA_VENC > eDATA_PAGO .AND. eDATA_PAGO < eMES_ANT
						 PLQUA("01")
						 PMENSAG("Calculo de Desconto, Aguarde ...")
						 eMULTA    = 0
						 eJUROS    = 0
						 eCORRECAO = 0
						 eAUX      = 1
						 eDIA_VENC = 0
						 eMES_VENC = 0
						 eANO_VENC = 0
						 eDIA_VENC = SUBSTR(DTOC(eDATA_VENC),1,2)
						 eMES_VENC = SUBSTR(DTOC(eDATA_VENC),4,2)
						 eANO_VENC = SUBSTR(DTOC(eDATA_VENC),7,4)
						 eDIA_PAGO = 0
						 eMES_PAGO = 0
						 eANO_PAGO = 0
						 eDIA_PAGO = SUBSTR(DTOC(eDATA_PAGO),1,2)
						 eMES_PAGO = SUBSTR(DTOC(eDATA_PAGO),4,2)
						 eANO_PAGO = SUBSTR(DTOC(eDATA_PAGO),7,4)
						 eAUX_VENC = CTOD("")
						 eDIA_VENC = VAL(eDIA_VENC)
						 eMES_VENC = VAL(eMES_VENC)
						 eANO_VENC = VAL(eANO_VENC)
						 DO WHILE .T.
							eMES_VENC = eMES_VENC - 1
							IF eMES_VENC = 0
							   eMES_VENC = 12
							   eANO_VENC = eANO_VENC - 1
							ENDIF
							eAUX      = eAUX + 1
							eTAM_DIA  = LENNUM(INT(eDIA_VENC))
							eTAM_MES  = LENNUM(INT(eMES_VENC))
							
							****
							* Tratamento para o Dia de Vencimento Fevereiro
							IF (eDIA_VENC > 28) .AND. (eMES_VENC = 2)
								eTEMP_VENC = CTOD("01/03/"+STR(INT(eANO_VENC),4))-1
								eDIA_VENC = VAL(SUBSTR(DTOC(eTEMP_VENC),1,2))
							ENDIF
							**** C¢digo Original                                    
							*eDIA_VENC = IIF(eDIA_VENC > 28 .AND. eMES_VENC = 2,28,eDIA_VENC)
							
							eDIA_VENC = IIF(eTAM_DIA = 1,"0"+STR(eDIA_VENC,1),STR(eDIA_VENC,2))
							eMES_VENC = IIF(eTAM_MES = 1,"0"+STR(eMES_VENC,1),STR(eMES_VENC,2))
							eANO_VENC = STR(INT(eANO_VENC),4)
							eAUX_VENC = CTOD(eDIA_VENC+"/"+eMES_VENC+"/"+eANO_VENC)
							eDIA_VENC = VAL(IIF(eTAM_DIA = 1,"0"+ALLTRIM(eDIA_VENC),ALLTRIM(eDIA_VENC)))
							eMES_VENC = VAL(IIF(eTAM_MES = 1,"0"+ALLTRIM(eMES_VENC),ALLTRIM(eMES_VENC)))
							eANO_VENC = VAL(eANO_VENC)
							IF MONTH(eAUX_VENC) = MONTH(eDATA_PAGO) .AND. YEAR(eAUX_VENC) = YEAR(eDATA_PAGO)
							   EXIT
							ENDIF
						 ENDDO
						 IF eAUX_VENC < eDATA_PAGO
							eAUX = eAUX - 1
						 ENDIF
						 * Calculo do desconto
						 eDESCONTO = (eVLR_PARC * (eTX_DESCON * eAUX))
						 eMULTA    = 0
						 eJUROS    = 0
						 eCORRECAO = 0
					  OTHERWISE
						 eDESCONTO = 0
						 eMULTA    = 0
						 eJUROS    = 0
						 eCORRECAO = 0
				   ENDCASE
			OTHERWISE
			   * Calculo de Multa e Juros
			   DO CASE
				  CASE eDATA_PAGO > eDATA_VENC
					 * Calculo de Multa e Juros, Aguarde ...3")
					 eDESCONTO = 0
					 eMULTA    = 0
					 eJUROS    = 0
					 eCORRECAO = 0
					 eAUX      = 1
					 eDESCONTO = 0
					 *Verifica se o pagamento ‚ fora ou dentro do Mˆs
					 IF (MONTH(eDATA_VENC) = MONTH(eDATA_PAGO)) .AND. (YEAR(eDATA_VENC) = YEAR(eDATA_PAGO))
						* Pagamento dentro do Mes
						eIND_MES  = 0                                     
						* Data de Vencimento
						eDIA_VENC = ""
						eMES_VENC = ""
						eANO_VENC = ""
						eDIA_VENC = ALLTRIM(SUBSTR(DTOC(eDATA_VENC),1,2))
						eMES_VENC = ALLTRIM(SUBSTR(DTOC(eDATA_VENC),4,2))
						eANO_VENC = ALLTRIM(SUBSTR(DTOC(eDATA_VENC),7,4))

						eTAM_DIA  = 0
						eTAM_MES  = 0
						eTAM_DIA  = LEN(eDIA_VENC)
						eTAM_MES  = LEN(eMES_VENC)

						eDIA_VENC = IIF(eTAM_DIA = 1,"0"+SUBSTR(eDIA_VENC,1,1),SUBSTR(eDIA_VENC,1,2))
						
						eMES_VENC = IIF(eTAM_MES = 1,"0"+SUBSTR(eMES_VENC,1,1),SUBSTR(eMES_VENC,1,2))
						
						eANO_VENC = SUBSTR(eANO_VENC,1,4)				    

						* Data de Pagamento                                    
						eDIA_PAGO = ""
						eMES_PAGO = ""
						eANO_PAGO = ""
						eDIA_PAGO = ALLTRIM(SUBSTR(DTOC(eDATA_PAGO),1,2))
						eMES_PAGO = ALLTRIM(SUBSTR(DTOC(eDATA_PAGO),4,2))
						eANO_PAGO = ALLTRIM(SUBSTR(DTOC(eDATA_PAGO),7,4))

						eTAM_DIA  = LEN(eDIA_PAGO)
						eTAM_MES  = LEN(eMES_PAGO)

						eDIA_PAGO = IIF(eTAM_DIA = 1,"0"+SUBSTR(eDIA_PAGO,1,1),SUBSTR(eDIA_PAGO,1,2))
						
						eMES_PAGO = IIF(eTAM_MES = 1,"0"+SUBSTR(eMES_PAGO,1,1),SUBSTR(eMES_PAGO,1,2))
						
						eANO_PAGO = SUBSTR(eANO_PAGO,1,4)

						* Data Contagem 
						eTAM_DIA  = 0
						eTAM_MES  = 0
						eTAM_DIA  = LEN(eDIA_VENC)
						eTAM_MES  = LEN(eMES_VENC)

						* Periodo para filtro taxa mensal                                
						eDIA_CONT2 = "01"
						eMES_CONT2 = " "
						eANO_CONT2 = " "
						IF VAL(eMES_VENC)-1 = 0
						   eMES_CONT2 = "12"
						   eANO_CONT2 = VAL(eANO_PAGO)-1
						   eANO_CONT2 = ALLTRIM(STR(eANO_CONT2))                                       
						ELSE
						   eMES_CONT2 = ALLTRIM(STR(VAL(eMES_VENC)-1))
						   eTAM_MES  = LEN(eMES_CONT2)
						   eMES_CONT2 = IIF(eTAM_MES = 1,"0"+SUBSTR(eMES_CONT2,1,1),SUBSTR(eMES_CONT2,1,2))
						   eANO_CONT2 = SUBSTR(eANO_VENC,1,4)                                       
						ENDIF
						
						* Periodo para contagem de dias do Mes
						eDIA_CONT = "01"
						eMES_CONT = " "
						eANO_CONT = " "
						IF VAL(eMES_VENC)+1 = 13
						   eMES_CONT = "01"
						   eANO_CONT = SUBSTR(STR(VAL(eANO_VENC)+1),1,4)                                       
						ELSE
						   eMES_CONT = ALLTRIM(STR(VAL(eMES_VENC)+1))
						   eTAM_MES  = LEN(eMES_CONT)
						   eMES_CONT = IIF(eTAM_MES = 1,"0"+SUBSTR(eMES_CONT,1,1),SUBSTR(eMES_CONT,1,2))
						   eANO_CONT = SUBSTR(eANO_VENC,1,4)                                       
						ENDIF
						
						eDATA_CONT = CTOD(eDIA_CONT+"/"+eMES_CONT+"/"+eANO_CONT)
						eDATA_CONT = eDATA_CONT - 1
						eDIF_DMES = VAL(SUBSTR(DTOC(eDATA_CONT),1,2))                

						eDIF_DTOTAL = 0
						eDIF_DTOTAL = eDATA_PAGO - eDATA_VENC
						eOBS2 = STR(eDIF_DTOTAL)+" DIAS ATRASO"

						* Calculo do Indice Acumulado
						IF (eINDICE <> "NENHUM")
							SELE 7
							GOTO TOP
							SET FILTER TO ((ANO+MES = eANO_CONT2+eMES_CONT2))
							GOTO TOP
							IF .NOT. EOF()
							   eIND_MES  = &eINDICE
							ELSE
							   eIND_MES  = 0
							ENDIF   
						ELSE
							   eIND_MES  = 0				    
						ENDIF

						* Calculo da Corre‡Æo
						*=1000+((2,5/31*10)*1000/100) Parcela
						*(¡ndice/dias do mˆs*dias de atraso)*parcela%)

						IF eIND_MES <> 0
						   eCORRECAO = (((eIND_MES/100)/eDIF_DMES)*eDIF_DTOTAL)*eVLR_PARC
						ELSE
						   eCORRECAO = 0
						ENDIF   

						SELE 8

						* Calculo do Juros 
						*=1008,06+((1/31*10)*1008,06/100)  Parcela corrigida+(juro 1%/dias do mˆs*dias de atraso)*parcela corrigida%)
						
						eJUROS = (((eTX_COMPER/100)/eDIF_DMES)*eDIF_DTOTAL)*(eVLR_PARC+eCORRECAO)
						
						* Calculo da Multa
						*=1011,32+(1011,32*2/100)  Parcela corrigida e com juro+(parcela corrigida e com juro% * multa 2%
						
						eMULTA = ((eVLR_PARC + eCORRECAO + eJUROS) * (eTX_MULTA / 100 ))
						
						* Prepara‡Æo do quadro de Pagamento
						eAUX_ACUM = eIND_MES

					 ELSE
						SELE 8 
						* Pagamento fora do Mes - VENCIMENTO
						eDIA_VENC = ""
						eMES_VENC = ""
						eANO_VENC = ""
						eDIA_VENC = ALLTRIM(SUBSTR(DTOC(eDATA_VENC),1,2))
						eMES_VENC = ALLTRIM(SUBSTR(DTOC(eDATA_VENC),4,2))
						eANO_VENC = ALLTRIM(SUBSTR(DTOC(eDATA_VENC),7,4))
						eTAM_DIA  = 0
						eTAM_MES  = 0
						eTAM_DIA  = LEN(eDIA_VENC)
						eTAM_MES  = LEN(eMES_VENC)
						eDIA_VENC = IIF(eTAM_DIA = 1,"0"+SUBSTR(eDIA_VENC,1,1),SUBSTR(eDIA_VENC,1,2))
						eMES_VENC = IIF(eTAM_MES = 1,"0"+SUBSTR(eMES_VENC,1,1),SUBSTR(eMES_VENC,1,2))
						eANO_VENC = SUBSTR(eANO_VENC,1,4)	
						
						* DATA CONTAGEM INICIAL 2 PARA MUDAR O INDICE PARA O MES ANTERIOR AO ATUAL
						eTAM_DIA  = 0
						eTAM_MES  = 0
						eTAM_DIA  = LEN(eDIA_VENC)
						eTAM_MES  = LEN(eMES_VENC)
						eTAM_ANO  = LEN(eANO_VENC)
					  
						eDIA_CONT2 = "01"
						eMES_CONT2 = " "
						eANO_CONT2 = " "
						IF VAL(eMES_VENC)-1 = 0
						   eMES_CONT2 = "12"
						   eANO_CONT2 = VAL(eANO_VENC)-1
						   eANO_CONT2 = ALLTRIM(STR(eANO_CONT2))
						ELSE
						   eMES_CONT2 = ALLTRIM(STR(VAL(eMES_VENC)-1))
						   eTAM_MES  = LEN(eMES_CONT2)
						   eMES_CONT2 = IIF(eTAM_MES = 1,"0"+SUBSTR(eMES_CONT2,1,1),SUBSTR(eMES_CONT2,1,2))
						   eANO_CONT2 = ALLTRIM(eANO_VENC)
						ENDIF

						* Data de Pagamento                                    
						eDIA_PAGO = ""
						eMES_PAGO = ""
						eANO_PAGO = ""
						eDIA_PAGO = ALLTRIM(SUBSTR(DTOC(eDATA_PAGO),1,2))
						eMES_PAGO = ALLTRIM(SUBSTR(DTOC(eDATA_PAGO),4,2))
						eANO_PAGO = ALLTRIM(SUBSTR(DTOC(eDATA_PAGO),7,4))

						eTAM_DIA  = LEN(eDIA_PAGO)
						eTAM_MES  = LEN(eMES_PAGO)

						eDIA_PAGO = IIF(eTAM_DIA = 1,"0"+SUBSTR(eDIA_PAGO,1,1),SUBSTR(eDIA_PAGO,1,2))
						eMES_PAGO = IIF(eTAM_MES = 1,"0"+SUBSTR(eMES_PAGO,1,1),SUBSTR(eMES_PAGO,1,2))
						eANO_PAGO = SUBSTR(eANO_PAGO,1,4)
						
						* DATA CONTAGEM INICIAL 3 PARA MUDAR O INDICE PARA O MES ANTERIOR AO MES DO PAGAMENTO
						eTAM_DIA  = 0
						eTAM_MES  = 0
						eTAM_DIA  = LEN(eDIA_PAGO)
						eTAM_MES  = LEN(eMES_PAGO)
						eTAM_ANO  = LEN(eANO_PAGO)
					  
						eDIA_CONT3 = "01"
						eMES_CONT3 = " "
						eANO_CONT3 = " "
						IF VAL(eMES_PAGO)-1 = 0
						   eMES_CONT3 = "12"
						   eANO_CONT3 = VAL(eANO_PAGO)-1
						   eANO_CONT3 = ALLTRIM(STR(eANO_CONT3))
						ELSE
						   eMES_CONT3 = ALLTRIM(STR(VAL(eMES_PAGO)-1))
						   eTAM_MES  = LEN(eMES_CONT3)
						   eMES_CONT3 = IIF(eTAM_MES = 1,"0"+SUBSTR(eMES_CONT3,1,1),SUBSTR(eMES_CONT3,1,2))
						   eANO_CONT3 = ALLTRIM(eANO_PAGO)
						ENDIF
					 
						* Contadores Datas
						eDIA_INIC = ""
						eMES_INIC = ""
						eANO_INIC = ""
						eDIA_INIC = ALLTRIM(SUBSTR(DTOC(eDATA_VENC),1,2))
						eMES_INIC = ALLTRIM(SUBSTR(DTOC(eDATA_VENC),4,2))
						eANO_INIC = ALLTRIM(SUBSTR(DTOC(eDATA_VENC),7,4))
						
						eDIA_FINA = ""
						eMES_FINA = ""
						eANO_FINA = ""
						eDIA_FINA = ALLTRIM(SUBSTR(DTOC(eDATA_PAGO),1,2))
						eMES_FINA = ALLTRIM(SUBSTR(DTOC(eDATA_PAGO),4,2))
						eANO_FINA = ALLTRIM(SUBSTR(DTOC(eDATA_PAGO),7,4))

						eCORRECAO = 0
						eCORRECAOANT = 0                                    
						eJUROS = 0
						eMULTA = 0
						eIND_MES  = 0                                       
						
						*SET PRINT ON
						*SET PRINT TO LOG.TXT

						SELE 7
						GOTO TOP
						SET FILTER TO ((ANO+MES >= eANO_CONT2+eMES_CONT2) .and. (ANO+MES <= eANO_CONT3+eMES_CONT3))				    
						*SET FILTER TO ((ANO+MES >= eANO_VENC+eMES_VENC) .and. (ANO+MES <= eANO_PAGO+eMES_PAGO))
					
						GOTO TOP
						DO WHILE .NOT. EOF()				    
						   IF (eINDICE <> "NENHUM")
							  eIND_MES  = &eINDICE
						   ELSE
							  eIND_MES  = 0
						   ENDIF   

						   IF (eIND_MES<>0)
							  * Data Contagem 1o. Mes
							  eTAM_DIA  = 0
							  eTAM_MES  = 0
							  eTAM_DIA  = LEN(eDIA_INIC)
							  eTAM_MES  = LEN(eMES_INIC)
							  eTAM_ANO  = LEN(eANO_INIC)
							  
							  eDIA_CONT = "01"
							  eMES_CONT = " "
							  eANO_CONT = " "
							  IF VAL(eMES_INIC)+1 = 13
								 eMES_CONT = "01"
								 eANO_CONT = VAL(eANO_INIC)+1
								 eANO_CONT = ALLTRIM(STR(eANO_CONT))
							  ELSE
								 eMES_CONT = ALLTRIM(STR(VAL(eMES_INIC)+1))
								 eTAM_MES  = LEN(eMES_CONT)
								 eMES_CONT = IIF(eTAM_MES = 1,"0"+SUBSTR(eMES_CONT,1,1),SUBSTR(eMES_CONT,1,2))
								 eANO_CONT = ALLTRIM(eANO_INIC)
							  ENDIF
							  
								*Ultimo Dia do Mes Corrente = Total Dias Mes
								eDATA_CONT = CTOD(eDIA_CONT+"/"+eMES_CONT+"/"+eANO_CONT)
								eDATA_CONT = eDATA_CONT - 1
								eDIF_DMES = VAL(SUBSTR(DTOC(eDATA_CONT),1,2))
								  
								eDATA_INIC = ""
								*Diferen‡a de Dias do Mes Corrente = Total Dias Devidos
								*Dias do Ultimo Mes
								IF (MONTH(eDATA_CONT) = MONTH(eDATA_PAGO)) .AND. (YEAR(eDATA_CONT) = YEAR(eDATA_PAGO)) 
								 eDIF_DTOTAL = 0
								 eDATA_INIC = CTOD(eDIA_CONT+"/"+eMES_CONT+"/"+eANO_CONT) 
								 eDATA_INIC = eDATA_INIC - 1                                             
								 eDATA_INIC = CTOD("01/"+SUBSTR(DTOC(eDATA_INIC),4,7))
								 eDIF_DTOTAL = eDATA_PAGO - eDATA_INIC
								 eDIF_DTOTAL = eDIF_DTOTAL + 1
								 eOBS2 = STR(eDIF_DTOTAL)+" DIAS ATRASO"							 
								 
								ELSE
									 IF (MONTH(eDATA_CONT) = MONTH(eDATA_VENC)) .AND. (YEAR(eDATA_CONT) = YEAR(eDATA_VENC)) 
										eDIF_DTOTAL = 0
										eDIF_DTOTAL = eDATA_CONT - eDATA_VENC
									 ELSE                                          
										eDIF_DTOTAL = 0
										eDATA_INIC = CTOD(eDIA_CONT+"/"+eMES_CONT+"/"+eANO_CONT)     
										eDATA_INIC = eDATA_INIC - 1
										eDATA_INIC = CTOD("01/"+SUBSTR(DTOC(eDATA_INIC),4,7))
										eDIF_DTOTAL = eDATA_CONT - eDATA_INIC
										eDIF_DTOTAL = eDIF_DTOTAL + 1
									 ENDIF   
								ENDIF

								IF (eINDICE <> "NENHUM")
								   eIND_MES  = &eINDICE
								ELSE     
								  eIND_MES  = 0
								ENDIF                                      

								* Calculo da Corre‡Æo
								*=1000+((2,5/31*10)*1000/100) Parcela
								*(¡ndice/dias do mˆs*dias de atraso)*parcela%) 
								eCORRECAOANT = 0
								eCORRECAOANT = eCORRECAO
								eCORRECAO = eCORRECAO + ((((eIND_MES)/eDIF_DMES)*eDIF_DTOTAL)/100)*(eVLR_PARC+eCORRECAOANT+eJUROS)

								*CODIGO ANTERIOR				          
								*eCORRECAO = eCORRECAO + (((eIND_MES/100)/eDIF_DMES)*eDIF_DTOTAL)*(eVLR_PARC+eCORRECAOANT)

								* Prepara‡Æo do quadro de Pagamento
								eAUX_ACUM = eAUX_ACUM * ((eIND_MES/100)+1)                                          

								SELE 8
								* Calculo do Juros 
								*=1008,06+((1/31*10)*1008,06/100)  Parcela corrigida+(juro 1%/dias do mˆs*dias de atraso)*parcela corrigida%)
								eJUROSANT = 0
								eJUROSANT = eJUROS
								eJUROS = eJUROS + ((((eTX_COMPER)/eDIF_DMES)*eDIF_DTOTAL)/100)*(eVLR_PARC+eCORRECAO+eJUROSANT)

								*eJUROS = eJUROS + (((eTX_COMPER/100)/eDIF_DMES)*eDIF_DTOTAL)*(eVLR_PARC+eCORRECAOANT+eJUROSANT)                                          

								*Muda DATA_INIC para o 1o. dia do Proximo Mes
								eDIA_INIC = ""
								eMES_INIC = ""
								eANO_INIC = ""
								eDATA_INIC = CTOD(eDIA_CONT+"/"+eMES_CONT+"/"+eANO_CONT)	                                  
								eDIA_INIC = ALLTRIM(SUBSTR(DTOC(eDATA_INIC),1,2))
								eMES_INIC = ALLTRIM(SUBSTR(DTOC(eDATA_INIC),4,2))
								eANO_INIC = ALLTRIM(SUBSTR(DTOC(eDATA_INIC),7,4))

							ENDIF
							SELE 7
							SKIP
							   
						ENDDO

						* Calculo da Multa
						*=1011,32+(1011,32*2/100)  Parcela corrigida e com juro+(parcela corrigida e com juro% * multa 2%
						eMULTA = ((eVLR_PARC + eCORRECAO + eJUROS) * (eTX_MULTA / 100 ))
										
						* Prepara‡Æo do quadro de Pagamento
						*eAUX_ACUM = ((eAUX_ACUM-1)*100) 
						eAUX_ACUM = 0
					 ENDIF                                 
				ENDCASE
			ENDCASE

			* Fim Calculo Terra SimÆo
		ENDIF		

		eVLR_CORR = ((eVLR_PARC + (eMULTA + eJUROS + eCORRECAO)) - eDESCONTO)
	   
		** Fim do Calculo da correção
		*********

         SELE 8
         @ LIN,90  SAY eVLR_CORR   PICT "@R 999,999,999,999.99"		
		 
		 *IF (OBS <> "")
			@ LIN,115 SAY OBS
		 *ELSE
			*@ LIN,115 SAY eOBS2
		 *ENDIF
		 
         eSUB_PARC  = eSUB_PARC + VLR_PARC
         eSUB_CORR  = eSUB_CORR + eVLR_CORR		 
         eSUB_TIT   = eSUB_TIT + 1
         LIN = LIN + 1
         SKIP
         IF eTOTALI
            IF eQUADRA = QUADRA .AND. eNUM_IMOVEL = NUM_IMOVEL .AND. ePARTE = PARTE


            ELSE
               @ LIN,02  SAY "SubTotal ("+ALLTRIM(eCLASSE)+"/"+ALLTRIM(eQUADRA)+"/"+ALLTRIM(eNUM_IMOVEL)+"/"+ALLTRIM(STR(ePARTE,1))+")   : "
               @ LIN,31  SAY eSUB_TIT    PICT "@R 999,999"
               @ LIN,44  SAY eSUB_PARC   PICT "@R 999,999,999,999.99"
               @ LIN,90  SAY eSUB_CORR   PICT "@R 999,999,999,999.99"			   
               LIN = LIN + 2
               eTOT_PARC   = eTOT_PARC  + eSUB_PARC
               eTOT_CORR   = eTOT_CORR  + eSUB_CORR			   
               eTOT_TIT    = eTOT_TIT   + eSUB_TIT
               eSUB_PARC   = 0
               eSUB_CORR   = 0			   
               eSUB_TIT    = 0
               eCLASSE     = CLASSE
               eQUADRA     = QUADRA
               eNUM_IMOVEL = NUM_IMOVEL
               ePARTE      = PARTE
            ENDIF
         ELSE
		   eTOT_PARC   = eTOT_PARC  + eSUB_PARC
		   eTOT_CORR   = eTOT_CORR  + eSUB_CORR			   
		   eTOT_TIT    = eTOT_TIT   + eSUB_TIT
		   eSUB_PARC   = 0
		   eSUB_CORR   = 0			   
		   eSUB_TIT    = 0
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
         @ LIN,90  SAY eTOT_CORR   PICT "@R 999,999,999,999.99"		 
         LIN = LIN + 2
         IF eDEVEDOR = "P"
            @ LIN,02  SAY "OBSERVACAO : RELATORIO DEVEDORES CORRIGIDO EXCLUINDO AS PARCELAS PAGAS"
         ELSE
            @ LIN,02  SAY "OBSERVACAO : RELATORIO DEVEDORES CORRIGIDO TOTAIS INCLUINDO AS PARCELAS PAGAS"
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
FUNCTION SELDEVA2
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
