*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : EXC_IMOV.PRG
* Data        : 01/01/99
* Objetivo    : EXCLUSAO DA VENDA DE UNIDADES IMOBILIARIAS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION EXC_IMOV()
SAVE SCREEN TO TELA03
PTIT( "CANCELAMENTO DA VENDA")
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
USE RENEGOCI INDEX RENEGOCI
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
         DBEDIT(6,14,6,75,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
      eQUADRA     = NUM_QUADRA
      eNUM_INICIO = NUM_INICIO
      eNUM_FINAL  = NUM_FINAL
      @ 07,02 SAY sisUNID1+" : "
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
      GOTO TOP
      IF EOF()
         PLQUA("01")
		 PMSGR( "Atená∆o !",3)
		 PMENSAG("N∆o H† "+sisUNID1+" Dispon°veis neste Produto.")
         PPARA()
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
         DBEDIT(7,15,7,69,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
         eNUM_IMOVEL = NUM_IMOVEL
         eSTATUS     = STATUS
         ePROPR_1    = PROPRIET_1
         ePROPR_2    = PROPRIET_2
         eQUITA_1    = QUITADO_1
         eQUITA_2    = QUITADO_2
         IF eSTATUS = 0
            IF ePROPR_1 = 1
               PLQUA("01")
               PMSGR( "Atená∆o !",3)
               PMENSAG ( sisUNID1+" N∆o Vendido !")
               PPARA()
            ELSE
               IF eQUITA_1
                  PLQUA("01")
                  PMSGR( "Atená∆o !",3)
                  PMENSAG ( sisUNID1+" Quitado, N∆o á Possável Excluir !")
                  PPARA()
               ELSE
                  ePARTE = 0
                  F_CANCV(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,0)
               ENDIF
            ENDIF
         ELSE
            eOPCAO = 0
            PLQUA("01")
            @ 23,00 SAY "Informe a Venda para o (1)o. ou (2)o. Meio Lote ? " GET eOPCAO PICT "99999" RANGE 1,2
            READ
            IF eOPCAO = 1
               IF ePROPR_1 = 1
                  PLQUA("01")
                  PMSGR( "Atená∆o !",3)
                  PMENSAG ( sisUNID1+" N∆o Vendido !")
                  PPARA()
               ELSE
                  IF eQUITA_1
                     PLQUA("01")
                     PMSGR( "Atená∆o !",3)
                     PMENSAG ( sisUNID1+" Quitado, N∆o á Possável Excluir !")
                     PPARA()
                  ELSE
                     ePARTE = 1
                     F_CANCV(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,1)
                  ENDIF
               ENDIF
            ELSE
               IF ePROPR_2 = 1
                  PLQUA("01")
                  PMSGR( "Atená∆o !",3)
                  PMENSAG ( sisUNID1+" N∆o Vendido !")
                  PPARA()
               ELSE
                  IF eQUITA_2
                     PLQUA("01")
                     PMSGR( "Atená∆o !",3)
                     PMENSAG ( sisUNID1+" Quitado, N∆o á Possável Excluir !")
                     PPARA()
                  ELSE
                     ePARTE = 2
                     F_CANCV(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,2)
                  ENDIF
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
      DBEDIT(6,08,6,76,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
      eBLOCO      = NUM_BLOCO
      eQUANTIDADE = QUANTIDADE
      @ 07,02 SAY sisUNID2+" : "
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
      GOTO TOP
      IF EOF()
         PLQUA("01")
		 PMSGR( "Atená∆o !",3)
		 PMENSAG("N∆o H† "+sisUNID2+" Dispon°veis neste Produto.")
         PPARA()
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
         DBEDIT(7,14,7,64,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
         eNUM_IMOVEL = NUM_IMOVEL
         eSTATUS     = STATUS
         ePROPR_1    = PROPRIET_1
         eQUITA_1    = QUITADO_1
         ePARTE      = 0
         IF ePROPR_1 = 1
            PLQUA("01")
            PMSGR( "Atená∆o !",3)
            PMENSAG ( sisUNID1+" N∆o Vendido !")
            PPARA()
         ELSE
            IF eQUITA_1
               PLQUA("01")
               PMSGR( "Atená∆o !",3)
               PMENSAG ( sisUNID1+" Quitado, N∆o á Possável Excluir !")
               PPARA()
            ELSE
               F_CANCV(eCLAS_PRO,eBLOCO,eNUM_IMOVEL,0)
            ENDIF
         ENDIF
      ENDIF
   CASE eTIPO_PRO = 3
      * Consulta de Salas Comerciais
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eBLOCO    = SPACE(5)
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
      DBEDIT(6,10,6,41,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
      eBLOCO      = NUM_BLOCO
      eQUANTIDADE = QUANTIDADE
      @ 07,02 SAY "Sala    : "
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
      GOTO TOP
      IF EOF()
         PLQUA("01")
		 PMSGR( "Atená∆o !",3)
		 PMENSAG("N∆o H† Imáveis Dispon°veis neste Empreendimento.")
         PPARA()
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
         DBEDIT(7,14,7,64,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
         eNUM_IMOVEL = NUM_IMOVEL
         eSTATUS     = STATUS
         ePROPR_1    = PROPRIET_1
         eQUITA_1    = QUITADO_1         
         ePARTE      = 0
         IF ePROPR_1 = 1
            PLQUA("01")
            PMSGR( "Atená∆o !",3)
            PMENSAG("Imovel N∆o Vendido !")
            PPARA()
         ELSE
            IF eQUITA_1
               PLQUA("01")
               PMSGR( "Atená∆o !",3)
               PMENSAG("Imovel Quitado, N∆o á Possável Excluir !")
               PPARA()
            ELSE
               F_CANCV(eCLAS_PRO,eBLOCO,eNUM_IMOVEL,0)
            ENDIF
         ENDIF
      ENDIF
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION f_CANCV
PARAMETERS pT_PRO,pQUADRA,pUNIDADE,pM_UNID
eMSG = "CANCELAMENTO DA VENDA DA UNIDADE No. "+ALLTRIM(pUNIDADE)+" - "+IIF(pM_UNID=0,"Proprietario Unico",IIF(pM_UNID=1,"1o. PROPRIETARIO","2o. PROPRIETARIO"))
PCENTRA( eMSG,9)
PLQUA("01")
PPERG ( "Confirma o Imovel (S/N) ? ")
IF sisRESP = "N"
   RETURN 0
ENDIF
PLQUA("01")
SELE 6
GOTO TOP
SEEK STR(pT_PRO,5)+pQUADRA+pUNIDADE+STR(pM_UNID,1)
eNUM_CLIE = NUM_CLIE
SELE 5
USE CONTATO INDEX NUM_CLIE,CONTATO
GOTO TOP
SEEK eNUM_CLIE
@ 11,02 SAY "Nome :" GET CLIENTE
CLEAR GETS
SELE 6
@ 13,02 SAY "Data da Venda : " GET DATA_VENDA
eVLR_IMOVEL = SINAL+INTERMEDIA+VLR_PARCEL
@ 14,02 SAY "Vlr. Imovel   : " GET eVLR_IMOVEL PICT "@R 999,999,999,999.99"
@ 15,02 SAY "Vlr. do Sinal : " GET SINAL       PICT "@R 999,999,999,999.99"
@ 16,02 SAY "Vlr. Intermed.: " GET INTERMEDIA  PICT "@R 999,999,999,999.99"
@ 17,02 SAY "Num. Parcelas : " GET PARCELAS    PICT "@R 9999"
@ 18,02 SAY "Vlr. da Parc. : " GET VLR_PARCEL  PICT "@R 999,999,999,999.99"
@ 19,02 SAY "1o. Vencimento: " GET PRI_VENC
@ 20,02 SAY "Indexador     : " GET INDICE
@ 20,35 say "Periodicidade : " GET PERIODO
@ 21,02 SAY "Corretor      : " GET CORRETOR
CLEAR GETS
PLQUA("01")
PMSGR( "Atená∆o !",3)
PPERG ( "Confirma a Exclusáo da Venda (S/N) ? ")
IF sisRESP = "S"
   PLQUA("01")
   PMENSAG("Aguarde ...")
   SELE 8
   eIND_CLAS = STR(pT_PRO,5)
   eIND_QUAD = pQUADRA
   eZERO = "000"
   eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   USE &eARQ INDEX &eIND1,&eIND2,&eIND3
   GOTO TOP
   SET FILTER TO CLASSE = STR(pT_PRO,5) .AND. QUADRA = pQUADRA .AND. NUM_IMOVEL = pUNIDADE .AND. PARTE = pM_UNID .AND. DATA_PGTO <> CTOD("")
   GOTO TOP
   IF .NOT. EOF()
      PLQUA("01")
      PMSGR( "Atená∆o !",3)
	  PMENSAG("Financiamento com Parcelas Pagas, Impossivel excluir...")
      PPARA()
	  RETURN 0
   ENDIF
   SET FILTER TO
   GOTO TOP
   IF F_BLOQA()
      DELETE ALL FOR CLASSE = STR(pT_PRO,5) .AND. QUADRA = pQUADRA .AND. NUM_IMOVEL = pUNIDADE .AND. PARTE = pM_UNID
      UNLOCK
   ENDIF
   SELE 6
   SEEK STR(pT_PRO,5)+pQUADRA+pUNIDADE+STR(pM_UNID,1)
   IF F_BLOQR()
      DELETE
      UNLOCK
   ENDIF
   SELE 9
   IF F_BLOQA()
      DELETE ALL FOR CLASSE = STR(pT_PRO,5) .AND. QUADRA = pQUADRA .AND. NUM_IMOVEL = pUNIDADE .AND. PARTE = pM_UNID
      UNLOCK
   ENDIF
   SELE 1
   IF F_BLOQR()
      DO CASE
         CASE pM_UNID = 0
            REPLACE PROPRIET_1 WITH 1
         CASE pM_UNID = 1
            REPLACE PROPRIET_1 WITH 1
         CASE pM_UNID = 2
            REPLACE PROPRIET_2 WITH 1
         OTHERWISE
            PLQUA("01")
			PMENSAG("ERRO GRAVE, CONTACTE O SUPORTE IMEDIATAMENTE, IMPRIMA A TELA")
            PPARA()
            PPARA()
      ENDCASE
      UNLOCK
   ENDIF
   F_LACESSO(sisUSUARIO,"VENDA DE IMOVEIS","CANCELAMENTO DE VENDA : "+ALLTRIM(STR(pT_PRO,5))+"/"+ALLTRIM(pQUADRA)+"/"+ALLTRIM(pUNIDADE)+"/"+ALLTRIM(STR(pM_UNID,1)))
   PLQUA("01")
   PMSGR( "Cancelamento Concluido !",3)
ENDIF
RETURN 0
*************************  fim  *********************************
