*********************************************************************
function ALT_PROD()
SAVE SCREEN TO TELA03
PTIT( "MANUTEN€ÇO DE UNIDADES IMOBILIARIAS")
SELE 1
USE UNI_IMOB INDEX UNI_IMOB
GO TOP
SELE 2
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
SELE 3
USE CAD_QUAD INDEX CAD_QUAD
GOTO TOP
SELE 4
USE CAD_APAR INDEX CAD_APAR
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
   DBEDIT(5,15,5,70,CAMPO,"KEYS_4",PIC,CAB," "," - "," ")
   eCLAS_PRO = CODIGO
   eTIPO_PRO = TIPO
   EXIT
ENDDO
* Selecao da rotina de Alteracao conforme o tipo de produto
DO CASE
   CASE eTIPO_PRO = 1
      * Alteracao de Loteamentos
      SELE 3
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      DO WHILE .T.
	 SELE 3
	 eQUADRA   = 0
         @ 06,02 SAY sisGRUPO1+"  : "
	 GO TOP
	 DECLARE CAMPO[4],PIC[4],CAB[4],LENS[4]
	 AFIELDS(CAMPO,.T.,LENS)
	 PIC[1]="@R99999"
	 PIC[2]="@R99999"
	 PIC[3]="@R99999"
	 PIC[4]="@S99999"
	 CAB[1]="    "
	 CAB[2]="    "
	 CAB[3]="    "
	 CAB[4]="    "
	 DBEDIT(6,14,6,45,CAMPO,"KEYS_4",PIC,CAB," "," - "," ")
	 eQUADRA     = NUM_QUADRA
	 eNUM_INICIO = NUM_INICIO
	 eNUM_FINAL  = NUM_FINAL
	 SELE 1
	 SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA
	 GOTO TOP
         @ 07,02 SAY sisUNID1+"  : "
	 GO TOP
	 DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
	 AFIELDS(CAMPO,.T.,LENS)
	 PIC[1]="@R99999"
	 PIC[2]="@R99999"
	 PIC[3]="@R99999"
	 CAB[1]="    "
	 CAB[2]="    "
	 CAB[3]="    "
	 DBEDIT(7,10,7,41,CAMPO,"KEYS_4",PIC,CAB," "," - "," ")
	 eNUM_IMOVEL = NUM_IMOVEL
	 eALT_STAT   = .F.
	 IF STATUS = 1 .AND. PROPRIET_1 <> 1 .AND. PROPRIET_2 <> 1
	    PLQUA("01")
	    PMSGR( "Aten‡Æo !",3)
            PMENSAG ( sisUNID1+" Vendida para 2 Compradores, Imposs¡vel Alterar ...")
	    PPARA()
	    EXIT
	 ELSE
	    IF STATUS = 0 .AND. PROPRIET_1 <> 1 .AND. PROPRIET_2 = 1
	       eALT_STAT = .T.
	    ENDIF
	 ENDIF
	 PLQUA("01")
         PPERG ( "Confirma a Alteracao do "+sisUNID1+" No. "+ALLTRIM(eNUM_IMOVEL)+" (S/N) ? ")
	 IF sisRESP = "S"
	    IF F_BLOQR()
	       SET COLOR TO W/N+
               @ 08,00 SAY "DADOS SOBRE "+sisUNID1
	       SET COLOR TO W/N,+W/R
	       eSTAT_ANT = STATUS
	       eSTATUS   = STATUS
	       @ 09,20
	       @ 09,20 SAY IIF(eSTATUS = 0," - Unico Comprador"," - 2 Compradores")
	       @ 09,02 SAY "Status (0/1)  : " GET eSTATUS PICT "9" RANGE 0,1
	       READ
	       @ 09,20
	       @ 09,20 SAY IIF(eSTATUS = 0," - Unico Comprador"," - 2 Compradores")
	       REPLACE STATUS WITH eSTATUS
	       IF eSTATUS = 0
		  SET COLOR TO W/N+
                  @ 10,00 SAY "DADOS SOBRE "+sisUNID1+" COMPLETO"
		  SET COLOR TO W/N,+W/R
	       ELSE
		  SET COLOR TO W/N+
                  @ 10,00 SAY "DADOS SOBRE "+sisUNID1+" 1/2"
                  SET COLOR TO W/N,+W/R
	       ENDIF
               @ 11,02 SAY "Copia Escritu.: " GET ESCRITU_01 PICT "@! X"
               @ 12,02 SAY "Metragem      : " GET METRAGEM_1 PICT "99999.99"
               @ 12,29 SAY "M2"
               @ 13,02 SAY "Inscricao     : " GET INSCRICA_1 PICT "@R 99.999.999.99.9"
               @ 14,02 SAY "Valor         : " GET VALOR_1    PICT "@R 999,999,999.99"
	       READ
	       IF eSTATUS <> 0
		  IF METRAGEM_2 = 0
		     REPLACE METRAGEM_2 WITH METRAGEM_1,;
			     INSCRICA_2 WITH INSCRICA_1
		  ENDIF
		  SET COLOR TO W/N+
                  @ 15,00 SAY "DADOS "+sisUNID1+" 2/2"
		  SET COLOR TO W/N,+W/R
                  @ 16,02 SAY "Copia Escritu.: " GET ESCRITU_02 PICT "@! X"
                  @ 17,02 SAY "Metragem      : " GET METRAGEM_2 PICT "99999.99"
                  @ 17,29 SAY "M2"
                  @ 18,02 SAY "Inscricao     : " GET INSCRICA_2 PICT "@R 99.999.999.99.9"
                  @ 19,02 SAY "Valor         : " GET VALOR_2    PICT "@R 999,999,999.99"
		  READ
	       ENDIF
	       UNLOCK
	    ENDIF
	    IF eSTAT_ANT = 0 .AND. eSTATUS = 1 .AND. eALT_STAT
	       PLQUA("01")
	       PMSGR( "Aten‡Æo !",3)
               PMENSAG("Aguarde, Alterando Atributos do "+sisUNID1+" j  Vendido ...")
	       SELE 5
	       USE CAD_VEND INDEX CAD_VEND,DATA_VEN
	       GOTO TOP
	       SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+"0"
	       IF F_BLOQR()
		  REPLACE PARTE WITH 1
		  UNLOCK
	       ENDIF
	       SELE 6
	       USE CAD_PARC INDEX CAD_PARC,DAT_PARC,DAT_PGTO
	       GOTO TOP
	       SET FILTER TO STR(eCLAS_PRO,5) = CLASSE .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = 0
	       GOTO TOP
	       IF F_BLOQA()
		  REPLACE ALL PARTE WITH 1
		  UNLOCK
	       ENDIF
	    ENDIF
	 ENDIF
	 PLQUA("01")
	 PPERG ( "Continua (S/N) ? ")
	 IF sisRESP = "S"
	    @ 06,00 CLEAR TO 21,79
	    LOOP
	 ELSE
	    EXIT
	 ENDIF
      ENDDO
   CASE eTIPO_PRO = 2
      * Alteracao de Apartamentos
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      DO WHILE .T.
	 SELE 4
	 eBLOCO    = 0
         @ 06,02 SAY  sisGRUPO2+" : "
	 GO TOP
	 DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
	 AFIELDS(CAMPO,.T.,LENS)
	 PIC[1]="@R99999"
	 PIC[2]="@R99999"
	 PIC[3]="@R99999"
	 CAB[1]="    "
	 CAB[2]="    "
	 CAB[3]="    "
	 DBEDIT(6,08,6,76,CAMPO,"KEYS_4",PIC,CAB," "," - "," ")         
	 eBLOCO      = NUM_BLOCO
	 eQUANTIDADE = QUANTIDADE
	 SELE 1
	 SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO
	 GOTO TOP
         @ 07,02 SAY sisUNID2+" : "
	 GO TOP
	 DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
	 AFIELDS(CAMPO,.T.,LENS)
	 PIC[1]="@R99999"
	 PIC[2]="@R99999"
	 PIC[3]="@R99999"
	 CAB[1]="    "
	 CAB[2]="    "
	 CAB[3]="    "
	 DBEDIT(7,10,7,41,CAMPO,"KEYS_4",PIC,CAB," "," - "," ")
	 eNUM_IMOVEL = NUM_IMOVEL
	 PLQUA("01")
         PPERG ( "Confirma a Alteracao "+sisUNID2+" No. "+ALLTRIM(eNUM_IMOVEL)+" (S/N) ? ")
	 IF sisRESP = "S"
	    IF F_BLOQR()
	       SET COLOR TO W/N+
               @ 08,00 SAY "DADOS "+sisUNID2
	       SET COLOR TO W/N,+W/R
               @ 11,02 SAY "Copia Escritu.: " GET ESCRITU_01 PICT "@! X"
               @ 12,02 SAY "Metragem      : " GET METRAGEM_1 PICT "99999.99"
               @ 12,29 SAY "M2"
               @ 13,02 SAY "Inscricao     : " GET INSCRICA_1 PICT "@R 99.999.999.99.9"
               @ 14,02 SAY "Valor         : " GET VALOR_1    PICT "@R 999,999,999.99"
	       READ
	       UNLOCK
	    ENDIF
	 ENDIF
	 PLQUA("01")
	 PPERG ( "Continua (S/N) ? ")
	 IF sisRESP = "S"
	    @ 06,00 CLEAR TO 21,79
	    LOOP
	 ELSE
	    EXIT
	 ENDIF
      ENDDO
   CASE eTIPO_PRO = 3
      * Alteracao de Salas Comerciais
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      DO WHILE .T.
	 SELE 4
	 eBLOCO    = 0
         @ 06,02 SAY sisGRUPO2+" : "
	 GO TOP
	 DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
	 AFIELDS(CAMPO,.T.,LENS)
	 PIC[1]="@R99999"
	 PIC[2]="@R99999"
	 PIC[3]="@R99999"
	 CAB[1]="    "
	 CAB[2]="    "
	 CAB[3]="    "
	 DBEDIT(6,10,6,41,CAMPO,"KEYS_4",PIC,CAB," "," - "," ")
	 eBLOCO      = NUM_BLOCO
	 eQUANTIDADE = QUANTIDADE
	 SELE 1
	 SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO
	 GOTO TOP
         @ 07,02 SAY sisUNID2+" : "
	 GO TOP
	 DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
	 AFIELDS(CAMPO,.T.,LENS)
	 PIC[1]="@R99999"
	 PIC[2]="@R99999"
	 PIC[3]="@R99999"
	 CAB[1]="    "
	 CAB[2]="    "
	 CAB[3]="    "
	 DBEDIT(7,10,7,41,CAMPO,"KEYS_4",PIC,CAB," "," - "," ")
	 eNUM_IMOVEL = NUM_IMOVEL
	 PLQUA("01")
         PPERG ( "Confirma a Alteracao da "+sisUNID2+" No. "+ALLTRIM(eNUM_IMOVEL)+" (S/N) ? ")
	 IF sisRESP = "S"
	    IF F_BLOQR()
	       SET COLOR TO W/N+
	       @ 08,00 SAY "DADOS DO SALA COMERCIAL"
	       SET COLOR TO W/N,+W/R
               @ 11,02 SAY "Copia Escritu.: " GET ESCRITU_01 PICT "@! X"
               @ 12,02 SAY "Metragem      : " GET METRAGEM_1 PICT "99999.99"
               @ 12,29 SAY "M2"
               @ 13,02 SAY "Inscricao     : " GET INSCRICA_1 PICT "@R 99.999.999.99.9"
               @ 14,02 SAY "Valor Unidade : " GET VALOR_1    PICT "@R 999,999,999.99"
	       READ
	       UNLOCK
	    ENDIF
	 ENDIF
	 PLQUA("01")
	 PPERG ( "Continua (S/N) ? ")
	 IF sisRESP = "S"
	    @ 06,00 CLEAR TO 21,79
	    LOOP
	 ELSE
	    EXIT
	 ENDIF
      ENDDO
   OTHERWISE
      * Alteracao de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************

