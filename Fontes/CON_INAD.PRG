*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_INAD.PRG
* Data        : 01/01/99
* Objetivo    : CONSULTA DE TOTAL DE INADIMPLENTES 
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CON_INAD()
SAVE SCREEN TO TELA03
PTIT( "CONSULTA TOTAL DE INADIMPLENTES")
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
   CASE eTIPO_PRO = 3
      * Consulta de Salas Comerciais
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA    = SPACE(5)
      PLQUA("01")
      PPERG ( "Deseja Selecionar o Bloco (S/N) ? ")
      IF sisRESP = "S"
         @ 06,02 SAY "Bloco   : "
         GO TOP
         DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
         AFIELDS(CAMPO,.T.,LENS)
         PIC[1]="@R99999"
         PIC[2]="@R99999"
         PIC[3]="@R99999"
         CAB[1]="    "
         CAB[2]="    "
         CAB[3]="    "
         DBEDIT(6,10,6,41,CAMPO,"FUNC_CONS",PIC,CAB," "," - "," ")
         eQUADRA     = NUM_BLOCO
         eQUANTIDADE = QUANTIDADE
      ENDIF
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
eDATA_INI   = CTOD("01/01/1980")
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
   SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. DATA_PGTO = CTOD("") .AND. ATIVA
   GOTO TOP
ELSE
   PLQUA("01")
   PPERG ( "Deseja Criar Novo Banco de Dados (S/N) ? ")
   IF sisRESP = "S"
      PLQUA("01")
      PMSGR( "Aten‡Æo !",3)
	  PMENSAG("Aguarde a montagem do Banco de Dados para ImpressÆo ...")
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
            APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. DATA_PGTO = CTOD("") .AND. ATIVA
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
            APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. DATA_PGTO = CTOD("") .AND. ATIVA
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
   ELSE
*      SET EXCLUSIVE ON
      SELE 8
      USE TEMP_PAR
      GOTO TOP
      INDEX ON CLASSE+QUADRA+NUM_IMOVEL+STR(PARTE,1) TO TEMP_PAR
      GOTO TOP
      USE TEMP_PAR INDEX TEMP_PAR
      GOTO TOP
*      SET EXCLUSIVE OFF
      @ 23,00
	  @ 23,00 SAY "INDEXADO !!!!"
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
   IF eCONTADOR < eINTER_INI .OR. eCONTADOR > eINTER_FIM
      SEEK eCLASSE+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
      DO WHILE (eCLASSE = CLASSE .AND. eQUADRA = QUADRA .AND. eNUM_IMOVEL = NUM_IMOVEL .AND. ePARTE = PARTE) .AND. .NOT. EOF()
      DELETE
      SKIP
      ENDDO
   ENDIF
   SELE 8
   GOTO TOP
   PACK
   GOTO TOP
ENDIF
SELE 8
GOTO TOP
IF EOF()
   PLQUA("01")
   PMSGR( "Aten‡Æo !",3)
   PMENSAG("NÆo H  Devedores Neste Per¡odo !")
   PPARA()
ELSE
   eDATA_PGTO = DATA_PGTO
   DECLARE vDEVEDOR[12],vVLR_DEVE[12]
   AFILL(vDEVEDOR,0)
   AFILL(vVLR_DEVE,0)
   eTOT_PARC  = 0
   eSUB_PARC  = 0
   eTOT_NPAR  = 0
   eNUM_ANTE  = 0
   DO WHILE .NOT. EOF()
      eQUADRA     = QUADRA
      eNUM_IMOVEL = NUM_IMOVEL
      ePARTE      = PARTE
      SELE 8
      eSUB_PARC  = eSUB_PARC  + VLR_PARC
      eTOT_NPAR  = eTOT_NPAR + 1
      PLQUA("01")
      @ 23,00 SAY ALLTRIM(STR(eCLAS_PRO,5))+"/"+QUADRA+"/"+NUM_IMOVEL
      SKIP
      IF STR(eCLAS_PRO,5) <> CLASSE .OR. eQUADRA <> QUADRA .OR. eNUM_IMOVEL <> NUM_IMOVEL .OR. PARTE <> ePARTE
         eTOT_PARC = eTOT_PARC + eSUB_PARC
         vDEVEDOR[IIF(eTOT_NPAR >= 1 .AND. eTOT_NPAR <= 10,eTOT_NPAR,11)] = vDEVEDOR[IIF(eTOT_NPAR >= 1 .AND. eTOT_NPAR <= 10,eTOT_NPAR,11)] + 1
         vVLR_DEVE[IIF(eTOT_NPAR >= 1 .AND. eTOT_NPAR <= 10,eTOT_NPAR,11)] = vVLR_DEVE[IIF(eTOT_NPAR >= 1 .AND. eTOT_NPAR <= 10,eTOT_NPAR,11)] + eSUB_PARC
         eSUB_PARC = 0
         eTOT_NPAR = 0
      ENDIF
      IF EOF()
         eTOT_CASO = 0
         LIN = 10
         eAUX = 1
         DO WHILE eAUX <= 11
            IF eAUX <= 10
               @ LIN,10   SAY "DEVEDORES COM   "+STR(eAUX,2)+" PARCELAS : "
               @ LIN,40   SAY vDEVEDOR[eAUX]    PICT "@R 9,999"
               @ LIN,50   SAY vVLR_DEVE[eAUX]   PICT "@R 999,999,999,999.99"
            ELSE
               @ LIN,10   SAY "DEVEDORES COM + 10 PARCELAS : "
               @ LIN,40   SAY vDEVEDOR[eAUX]    PICT "@R 9,999"
               @ LIN,50   SAY vVLR_DEVE[eAUX]   PICT "@R 999,999,999,999.99"
            ENDIF
            eTOT_CASO = eTOT_CASO + vDEVEDOR[eAUX]
            LIN = LIN + 1
            eAUX = eAUX + 1
         ENDDO
         @ 21,10  SAY "TOTAL GERAL                 : " 
         @ 21,40  SAY eTOT_CASO PICT "@R 9,999"
         @ 21,50  GET eTOT_PARC PICT "@R 999,999,999,999.99"
         CLEAR GETS
      ENDIF
   ENDDO
   PMENSAG("Pressione Qualquer Tecla Para Terminar ...")
   PPARA() 
ENDIF
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
