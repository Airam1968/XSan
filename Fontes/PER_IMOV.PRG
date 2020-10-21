*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : PER_IMOV.PRG
* Data        : 01/01/99
* Objetivo    : PERMUTA DE UNIDADES IMOBILIARIAS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION PER_IMOV()
SAVE SCREEN TO TELA03
PTIT( "PERMUTA DE UNIDADE IMOBILIARIAS")
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
USE RENEGOCI INDEX RENEGOCI
GOTO TOP
SELE 8
* Cadastro de Parcelas
SELE 9
USE CAD_QUIT INDEX CAD_QUIT
GOTO TOP
SELE 2
PLIMPA()
PCENTRA( sisUNID1+" Origem",5)
* Selecao do produto
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   eCLAS_PRO = 0
   eTIPO_PRO = 0
   @ 07,02 SAY "Produto : "
   GO TOP
   DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1]="@R99999"
   PIC[2]="@R99999"
   PIC[3]="@S40"
   CAB[1]="    "
   CAB[2]="    "
   CAB[3]="    "
   DBEDIT(7,15,7,70,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
   eCLAS_PRO = CODIGO
   eTIPO_PRO = TIPO
   EXIT
ENDDO
* Selecao da rotina de Alteracao conforme o tipo de produto
eQUADRA = SPACE(1)
eBLOCO  = SPACE(1)
DO CASE
   CASE eTIPO_PRO = 1
      * Consulta de Loteamentos
      SELE 3
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA   = SPACE(5)
      @ 08,02 SAY sisGRUPO1+" : "
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
         DBEDIT(8,14,8,75,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
      eQUADRA     = NUM_QUADRA
      eNUM_INICIO = NUM_INICIO
      eNUM_FINAL  = NUM_FINAL
      @ 09,02 SAY sisUNID1+" : "
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
      GOTO TOP
      IF EOF()
         PLQUA("01")
		 PMSGR( "Atená∆o !",3)
		 PMENSAG(" N∆o H†  "+sisUNID1+"  Dispon°veis  neste  Produto. ")
         PPARA()
         CLOSE DATABASES
         RESTORE SCREEN FROM TELA03
         RETURN nil
      ELSE
         DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
         AFIELDS(CAMPO,.T.,LENS)
         CAMPO[5]="PROPRIET_1"
         CAMPO[6]="PROPRIET_2"
         LENS[5] =5
         LENS[6] =5
         PIC[1]  ="@R99999"
         PIC[2]  ="@R99999"
         PIC[3]  ="@R99999"
         PIC[4]  ="@R99999"
         PIC[5]  ="@R99999"
         PIC[6]  ="@R99999"
         CAB[1]  ="     "
         CAB[2]  ="     "
         CAB[3]  ="     "
         CAB[4]  ="     "
         CAB[5]  ="     "
         CAB[6]  ="     "
         DBEDIT(9,15,9,69,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
         eNUM_IMOVEL = NUM_IMOVEL
         eSTAT_ANT     = STATUS
         ePROPR_1    = PROPRIET_1
         ePROPR_2    = PROPRIET_2
         IF eSTAT_ANT = 0
            IF ePROPR_1 = 1
               PLQUA("01")
               PMSGR( "Atená∆o !",3)
               PMENSAG ( sisUNID1+" N∆o Vendido !")
               PPARA()
               CLOSE DATABASES
               RESTORE SCREEN FROM TELA03
               RETURN nil
            ELSE
               eCLAS_ANT = eCLAS_PRO
               eQUAD_ANT = eQUADRA
               eIMOV_ANT = eNUM_IMOVEL
               ePART_ANT = 0
            ENDIF
         ELSE
            eOPCAO = 0
            PLQUA("01")
            @ 23,00 SAY "Informe a Permuta para o (1)o. ou (2)o. Meio Lote ? " GET eOPCAO PICT "99999" RANGE 1,2
            READ
            IF eOPCAO = 1
               IF ePROPR_1 = 1
                  PLQUA("01")
                  PMSGR( "Atená∆o !",3)
                  PMENSAG ( sisUNID1+" N∆o Vendido !")
                  PPARA()
                  CLOSE DATABASES
                  RESTORE SCREEN FROM TELA03
                  RETURN nil
               ELSE
                  eCLAS_ANT = eCLAS_PRO
                  eQUAD_ANT = eQUADRA
                  eIMOV_ANT = eNUM_IMOVEL
                  ePART_ANT = 1
               ENDIF
            ELSE
               IF ePROPR_2 = 1
                  PLQUA("01")
                  PMSGR( "Atená∆o !",3)
                  PMENSAG ( sisUNID1+" N∆o Vendido !")
                  PPARA()
                  CLOSE DATABASES
                  RESTORE SCREEN FROM TELA03
                  RETURN nil
               ELSE
                  eCLAS_ANT = eCLAS_PRO
                  eQUAD_ANT = eQUADRA
                  eIMOV_ANT = eNUM_IMOVEL
                  ePART_ANT = 2
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   CASE eTIPO_PRO = 2
      * Consulta de Apartamentos
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eBLOCO    = SPACE(5)
      @ 08,02 SAY sisGRUPO2+" : "
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
      DBEDIT(8,08,8,76,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
      eBLOCO      = NUM_BLOCO
      eQUANTIDADE = QUANTIDADE
      @ 09,02 SAY sisUNID2+" : "
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
      GOTO TOP
      IF EOF()
         PLQUA("01")
		 PMSGR( "Atená∆o !",3)
		 PMENSAG(" N∆o H†  "+sisUNID2+"  Dispon°veis  neste  Produto .")
         PPARA()
         CLOSE DATABASES
         RESTORE SCREEN FROM TELA03
         RETURN nil
      ELSE
         DECLARE CAMPO[5],PIC[5],CAB[5],LENS[5]
         AFIELDS(CAMPO,.T.,LENS)
         CAMPO[5]="PROPRIET_1"
         LENS[5] =5
         PIC[1]="@R99999"
         PIC[2]="@R99999"
         PIC[3]="@R99999"
         PIC[4]="@R99999"
         PIC[5]="@R99999"
         CAB[1]=" Produto "
         CAB[2]="Grupo"
         CAB[3]="Unid."
         CAB[4]="Status"
         CAB[5]="Proprietario"
         DBEDIT(9,14,9,64,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
         eNUM_IMOVEL = NUM_IMOVEL
         eSTAT_ANT     = STATUS
         ePROPR_1    = PROPRIET_1
         ePARTE      = 0
         IF ePROPR_1 = 1
            PLQUA("01")
            PMSGR( "Atená∆o !",3)
            PMENSAG ( sisUNID2+" N∆o Vendido !")
            PPARA()
            CLOSE DATABASES
            RESTORE SCREEN FROM TELA03
            RETURN nil
         ELSE
            eCLAS_ANT = eCLAS_PRO
            eQUAD_ANT = eBLOCO 
            eIMOV_ANT = eNUM_IMOVEL
            ePART_ANT = 0
         ENDIF
      ENDIF
   CASE eTIPO_PRO = 3
      * Consulta de Salas Comerciais
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eBLOCO    = SPACE(5)
      @ 08,02 SAY "Bloco   : "
      GO TOP
      DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
      AFIELDS(CAMPO,.T.,LENS)
      PIC[1]="@R99999"
      PIC[2]="@R99999"
      PIC[3]="@R99999"
      CAB[1]="    "
      CAB[2]="    "
      CAB[3]="    "
      DBEDIT(8,10,8,41,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
      eBLOCO      = NUM_BLOCO
      eQUANTIDADE = QUANTIDADE
      @ 09,02 SAY "Sala    : "
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
      GOTO TOP
      IF EOF()
         PLQUA("01")
		 PMSGR( "Atená∆o !",3)
		 PMENSAG("N∆o H† Imáveis Dispon°veis neste Empreendimento.")
         PPARA()
         CLOSE DATABASES
         RESTORE SCREEN FROM TELA03
         RETURN nil
      ELSE
         DECLARE CAMPO[5],PIC[5],CAB[5],LENS[5]
         AFIELDS(CAMPO,.T.,LENS)
         CAMPO[5]="PROPRIET_1"
         LENS[5] =5
         PIC[1]="@R99999"
         PIC[2]="@R99999"
         PIC[3]="@R99999"
         PIC[4]="@R99999"
         PIC[5]="@R99999"
         CAB[1]="Empreend."
         CAB[2]="Bloco"
         CAB[3]="Sala"
         CAB[4]="Status"
         CAB[5]="Proprietario"
         DBEDIT(9,14,9,64,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
         eNUM_IMOVEL = NUM_IMOVEL
         eSTAT_ANT     = STATUS
         ePROPR_1    = PROPRIET_1
         ePARTE      = 0
         IF ePROPR_1 = 1
            PLQUA("01")
			PMSGR( "Atencáo !",3)
			PMENSAG("Imovel N∆o Vendido !")
            PPARA()
            CLOSE DATABASES
            RESTORE SCREEN FROM TELA03
            RETURN nil
         ELSE
            eCLAS_ANT = eCLAS_PRO
            eQUAD_ANT = eBLOCO 
            eIMOV_ANT = eNUM_IMOVEL
            ePART_ANT = 0
         ENDIF
      ENDIF
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
SELE 2
PCENTRA( "Imovel Destino",11)
* Selecao do produto
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   eCLAS_PRO = 0
   eTIPO_PRO = 0
   @ 13,02 SAY "Produto : "
   GO TOP
   DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1]="@R99999"
   PIC[2]="@R99999"
   PIC[3]="@S40"
   CAB[1]="    "
   CAB[2]="    "
   CAB[3]="    "
   DBEDIT(13,15,13,70,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
   eCLAS_PRO = CODIGO
   eTIPO_PRO = TIPO
   EXIT
ENDDO
* Selecao da rotina de Alteracao conforme o tipo de produto
eQUADRA = SPACE(1)
eBLOCO  = SPACE(1)
DO CASE
   CASE eTIPO_PRO = 1
      * Consulta de Loteamentos
      SELE 3
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA   = SPACE(5)
      @ 14,02 SAY "Quadra  : "
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
         DBEDIT(14,14,14,75,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
      eQUADRA     = NUM_QUADRA
      eNUM_INICIO = NUM_INICIO
      eNUM_FINAL  = NUM_FINAL
      @ 15,02 SAY "Lote    : "
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 = 1),(PROPRIET_1 = 1 .OR. PROPRIET_2 = 1))
      GOTO TOP
      IF EOF()
         PLQUA("01")
		 PMSGR( "Atená∆o !",3)
		 PMENSAG("N∆o H† Imáveis Dispon°veis neste Empreendimento.")
         PPARA()
         CLOSE DATABASES
         RESTORE SCREEN FROM TELA03
         RETURN nil
      ELSE
         DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
         AFIELDS(CAMPO,.T.,LENS)
         CAMPO[5]="PROPRIET_1"
         CAMPO[6]="PROPRIET_2"
         LENS[5] =5
         LENS[6] =5
         PIC[1]  ="@R99999"
         PIC[2]  ="@R99999"
         PIC[3]  ="@R99999"
         PIC[4]  ="@R99999"
         PIC[5]  ="@R99999"
         PIC[6]  ="@R99999"
         CAB[1]  ="     "
         CAB[2]  ="     "
         CAB[3]  ="     "
         CAB[4]  ="     "
         CAB[5]  ="     "
         CAB[6]  ="     "
         DBEDIT(15,15,15,69,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
         eNUM_IMOVEL = NUM_IMOVEL
         eSTAT_ATU     = STATUS
         ePROPR_1    = PROPRIET_1
         ePROPR_2    = PROPRIET_2
         IF eSTAT_ATU = 0
            IF ePROPR_1 <> 1
               PLQUA("01")
               PMSGR( "Atená∆o !",3)
               PMENSAG("Imovel j† Vendido !")
               PPARA()
               CLOSE DATABASES
               RESTORE SCREEN FROM TELA03
               RETURN nil
            ELSE
               eCLAS_ATU = eCLAS_PRO
               eQUAD_ATU = eQUADRA
               eIMOV_ATU = eNUM_IMOVEL
               ePART_ATU = 0
            ENDIF
         ELSE
            eOPCAO = 0
            PLQUA("01")
            @ 23,00 SAY "Informe a Permuta para o (1)o. ou (2)o. Meio Lote ? " GET eOPCAO PICT "99999" RANGE 1,2
            READ
            IF eOPCAO = 1
               IF ePROPR_1 <> 1
                  PLQUA("01")
                  PMSGR( "Atená∆o !",3)
                  PMENSAG("Imovel já Vendido !")
                  PPARA()
                  CLOSE DATABASES
                  RESTORE SCREEN FROM TELA03
                  RETURN nil
               ELSE
                  eCLAS_ATU = eCLAS_PRO
                  eQUAD_ATU = eQUADRA
                  eIMOV_ATU = eNUM_IMOVEL
                  ePART_ATU = 1
               ENDIF
            ELSE
               IF ePROPR_2 <> 1
                  PLQUA("01")
                  PMSGR( "Atená∆o !",3)
                  PMENSAG("Imovel já Vendido !")
                  PPARA()
                  CLOSE DATABASES
                  RESTORE SCREEN FROM TELA03
                  RETURN nil
               ELSE
                  eCLAS_ATU = eCLAS_PRO
                  eQUAD_ATU = eQUADRA
                  eIMOV_ATU = eNUM_IMOVEL
                  ePART_ATU = 2
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   CASE eTIPO_PRO = 2
      * Consulta de Apartamentos
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eBLOCO    = SPACE(5)
      @ 14,02 SAY "Bloco   : "
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
      DBEDIT(14,08,14,76,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
      eBLOCO      = NUM_BLOCO
      eQUANTIDADE = QUANTIDADE
      @ 15,02 SAY "Apto.   : "
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO .AND. IIF(STATUS = 0,(PROPRIET_1 = 1),(PROPRIET_1 = 1 .OR. PROPRIET_2 = 1))
      GOTO TOP
      IF EOF()
         PLQUA("01")
		 PMSGR( "Atená∆o !",3)
		 PMENSAG("N∆o H† Imáveis Dispon°veis neste Empreendimento.")
         PPARA()
         CLOSE DATABASES
         RESTORE SCREEN FROM TELA03
         RETURN nil
      ELSE
         DECLARE CAMPO[5],PIC[5],CAB[5],LENS[5]
         AFIELDS(CAMPO,.T.,LENS)
         CAMPO[5]="PROPRIET_1"
         LENS[5] =5
         PIC[1]="@R99999"
         PIC[2]="@R99999"
         PIC[3]="@R99999"
         PIC[4]="@R99999"
         PIC[5]="@R99999"
         CAB[1]="Empreend."
         CAB[2]="Bloco"
         CAB[3]="Apto."
         CAB[4]="Status"
         CAB[5]="Proprietario"
         DBEDIT(15,14,15,64,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
         eNUM_IMOVEL = NUM_IMOVEL
         eSTAT_ATU   = STATUS
         ePROPR_1    = PROPRIET_1
         ePARTE      = 0
         IF ePROPR_1 <> 1
            PLQUA("01")
            PMSGR( "Atená∆o !",3)
            PMENSAG("Imovel já Vendido !")
            PPARA()
            CLOSE DATABASES
            RESTORE SCREEN FROM TELA03
            RETURN nil
         ELSE
            eCLAS_ATU = eCLAS_PRO
            eQUAD_ATU = eBLOCO 
            eIMOV_ATU = eNUM_IMOVEL
            ePART_ATU = 0
         ENDIF
      ENDIF
   CASE eTIPO_PRO = 3
      * Consulta de Salas Comerciais
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eBLOCO    = SPACE(5)
      @ 14,02 SAY "Bloco   : "
      GO TOP
      DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
      AFIELDS(CAMPO,.T.,LENS)
      PIC[1]="@R99999"
      PIC[2]="@R99999"
      PIC[3]="@R99999"
      CAB[1]="    "
      CAB[2]="    "
      CAB[3]="    "
      DBEDIT(14,10,14,41,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
      eBLOCO      = NUM_BLOCO
      eQUANTIDADE = QUANTIDADE
      @ 15,02 SAY "Sala    : "
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO .AND. IIF(STATUS = 0,(PROPRIET_1 = 1),(PROPRIET_1 = 1 .OR. PROPRIET_2 = 1))
      GOTO TOP
      IF EOF()
         PLQUA("01")
		 PMSGR( "Atená∆o !",3)
		 PMENSAG("N∆o H† Imáveis Dispon°veis neste Empreendimento.")
         PPARA()
         CLOSE DATABASES
         RESTORE SCREEN FROM TELA03
         RETURN nil
      ELSE
         DECLARE CAMPO[5],PIC[5],CAB[5],LENS[5]
         AFIELDS(CAMPO,.T.,LENS)
         CAMPO[5]="PROPRIET_1"
         LENS[5] =5
         PIC[1]="@R99999"
         PIC[2]="@R99999"
         PIC[3]="@R99999"
         PIC[4]="@R99999"
         PIC[5]="@R99999"
         CAB[1]="Empreend."
         CAB[2]="Bloco"
         CAB[3]="Sala"
         CAB[4]="Status"
         CAB[5]="Proprietario"
         DBEDIT(15,14,15,64,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
         eNUM_IMOVEL = NUM_IMOVEL
         eSTAT_ATU   = STATUS
         ePROPR_1    = PROPRIET_1
         ePARTE      = 0
         IF ePROPR_1 <> 1
            PLQUA("01")
            PMSGR( "Atená∆o !",3)
            PMENSAG("Imovel já Vendido !")
            PPARA()
            CLOSE DATABASES
            RESTORE SCREEN FROM TELA03
            RETURN nil
         ELSE
            eCLAS_ATU = eCLAS_PRO
            eQUAD_ATU = eBLOCO 
            eIMOV_ATU = eNUM_IMOVEL
            ePART_ATU = 0
         ENDIF
      ENDIF
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
IF eCLAS_ANT = eCLAS_ATU .AND. eQUAD_ANT = eQUAD_ATU .AND. eIMOV_ANT = eIMOV_ATU .AND. ePART_ANT = ePART_ATU
   PLQUA("01")
   PMSGR( "Atená∆o !",3)
   PMENSAG("N∆o á Possável Permutar o Imovel com ele Mesmo ...")
   PPARA()
ELSE
   PLQUA("01")
   PPERG ( "Confirma a Permuta (S/N) ? ")
   IF sisRESP = "S"
      SELE 6
      SEEK STR(eCLAS_ANT,5)+eQUAD_ANT+eIMOV_ANT+STR(ePART_ANT,1)
      eCLIE_ANT = NUM_CLIE
      IF F_BLOQR()
         REPLACE CLASSE     WITH STR(eCLAS_ATU,5),;
                 QUADRA     WITH eQUAD_ATU,;
                 NUM_IMOVEL WITH eIMOV_ATU
         REPLACE PARTE      WITH ePART_ATU
         UNLOCK
      ENDIF
      SELE 8
      USE
      eIND_CLAS = STR(eCLAS_ANT,5)
      eIND_QUAD = eQUAD_ANT
      eZERO = "000"
      eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      SET EXCLUSIVE ON
      SELE 8
      USE TEMP_PAR
      GOTO TOP
      IF F_BLOQA()
         ZAP
         APPEND FROM &eARQ FOR CLASSE = STR(eCLAS_ANT,5) .AND. QUADRA = eQUAD_ANT .AND. NUM_IMOVEL = eIMOV_ANT .AND. PARTE = ePART_ANT
         GOTO TOP
         IF F_BLOQA()
            USE &eARQ INDEX &eIND1,&eIND2,&eIND3
            GOTO TOP
            DELETE ALL FOR CLASSE = STR(eCLAS_ANT,5) .AND. QUADRA = eQUAD_ANT .AND. NUM_IMOVEL = eIMOV_ANT .AND. PARTE = ePART_ANT
            UNLOCK
         ENDIF
         GOTO TOP
         IF F_BLOQA()
            USE TEMP_PAR
            GOTO TOP
            REPLACE ALL CLASSE     WITH STR(eCLAS_ATU,5),;
                        QUADRA     WITH eQUAD_ATU,;
                        NUM_IMOVEL WITH eIMOV_ATU
            REPLACE ALL PARTE      WITH ePART_ATU
            UNLOCK
         ENDIF
         eIND_CLAS = STR(eCLAS_ATU,5)
         eIND_QUAD = eQUAD_ATU
         eZERO = "000"
         eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
         eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
         eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
         eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
         USE 
         USE &eARQ INDEX &eIND1,&eIND2,&eIND3
         GOTO TOP
         IF F_BLOQA()
            APPEND FROM TEMP_PAR 
            GOTO TOP
            UNLOCK
         ENDIF
      ENDIF
      SET EXCLUSIVE OFF
      SELE 1
      SET FILTER TO
      GOTO TOP
      SEEK STR(eCLAS_ANT,5)+eQUAD_ANT+eIMOV_ANT+STR(ePART_ANT,1)
      IF F_BLOQR()
         IF ePART_ANT = 0 .OR. ePART_ANT = 1
            REPLACE PROPRIET_1 WITH 1
         ELSE
            REPLACE PROPRIET_2 WITH 1
         ENDIF
         UNLOCK
      ENDIF
      GOTO TOP
      SEEK STR(eCLAS_ATU,5)+eQUAD_ATU+eIMOV_ATU+STR(ePART_ATU,1)
      IF F_BLOQR()
         IF ePART_ATU = 1 .OR. ePART_ATU = 0
            REPLACE PROPRIET_1 WITH eCLIE_ANT
         ELSE
            REPLACE PROPRIET_2 WITH eCLIE_ANT
         ENDIF
         UNLOCK
      ENDIF
      PLQUA("01")
      PMSGR( "Permuta Concluáda !",3)
      F_LACESSO(sisUSUARIO,"ROTINAS ESPECIAIS","PERMUTA IMOVEL : "+ALLTRIM(STR(eCLAS_ANT,5))+"/"+ALLTRIM(eQUAD_ANT)+"/"+ALLTRIM(eIMOV_ANT)+"/"+ALLTRIM(STR(ePART_ANT,1))+; 
                                                        " PARA : "+ALLTRIM(STR(eCLAS_ATU,5))+"/"+ALLTRIM(eQUAD_ATU)+"/"+ALLTRIM(eIMOV_ATU)+"/"+ALLTRIM(STR(ePART_ATU,1)))
   ENDIF 
ENDIF
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
