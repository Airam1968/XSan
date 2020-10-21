*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : DES_IMOV.PRG
* Data        : 01/01/99
* Objetivo    : DESISTENCIA DE UNIDADES IMOBILIARIAS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION DES_IMOV()
SAVE SCREEN TO TELA03
PTIT( "DESISTÒNCIA DE VENDA")
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
* Selecao do produto
DO WHILE .T.
   PLIMPA()
   SET COLOR TO W/N,+W/R
   eCLAS_PRO = 0
   eTIPO_PRO = 0
   @ 05,02 SAY "Produto  : "
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
		 PMSGR( "Aten‡Æo !",3)
		 PMENSAG("NÆo H  "+sisUNID1+" Dispon¡veis neste Produto.")
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
         IF eSTATUS = 0
            IF ePROPR_1 = 1
               PLQUA("01")
               PMSGR( "Aten‡Æo !",3)
               PMENSAG ( sisUNID1+" NÆo Vendido !")
               PPARA()
            ELSE
               ePARTE = 0
               F_DESIS(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,0)
            ENDIF
         ELSE
            eOPCAO = 0
            PLQUA("01")
            @ 23,00 SAY "Informe a Venda para o (1)o. ou (2)o. Meio Lote ? " GET eOPCAO PICT "99999" RANGE 1,2
            READ
            IF eOPCAO = 1
               IF ePROPR_1 = 1
                  PLQUA("01")
                  PMSGR( "Aten‡Æo !",3)
                  PMENSAG ( sisUNID1+" NÆo Vendido !")
                  PPARA()
               ELSE
                  ePARTE = 1
                  F_DESIS(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,1)
               ENDIF
            ELSE
               IF ePROPR_2 = 1
                  PLQUA("01")
                  PMSGR( "Aten‡Æo !",3)
                  PMENSAG ( sisUNID2+" NÆo Vendido !")
                  PPARA()
               ELSE
                  ePARTE = 2
                  F_DESIS(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,2)
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
		 PMSGR( "Aten‡Æo !",3)
		 PMENSAG("NÆo H  "+sisUNID2+" Dispon¡veis neste Produto.")
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
         ePARTE      = 0
         IF ePROPR_1 = 1
            PLQUA("01")
            PMSGR( "Aten‡Æo !",3)
            PMENSAG ( sisUNID2+" NÆo Vendido !")
            PPARA()
         ELSE
            F_DESIS(eCLAS_PRO,eBLOCO,eNUM_IMOVEL,0)
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
		 PMSGR( "Aten‡Æo !",3)
		 PMENSAG("NÆo H  Im‡veis Dispon¡veis neste Empreendimento.")
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
         ePARTE      = 0
         IF ePROPR_1 = 1
            PLQUA("01")
            PMSGR( "Aten‡Æo !",3)
            PMENSAG("Imovel NÆo Vendido !")
            PPARA()
         ELSE
            F_DESIS(eCLAS_PRO,eBLOCO,eNUM_IMOVEL,0)
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
FUNCTION F_DESIS
PARAMETERS pT_PRO,pQUADRA,pUNIDADE,pM_UNID
eMSG = "DESIST‡NCIA DA VENDA DA UNIDADE No. "+ALLTRIM(pUNIDADE)+" - "+IIF(pM_UNID=0,"Proprietario Unico",IIF(pM_UNID=1,"1o. PROPRIETARIO","2o. PROPRIETARIO"))
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
PLQUA("01")
PMENSAG("Aguarde a Confirma‡Æo das Parcelas ...")
SELE 8
eIND_CLAS = STR(pT_PRO,5)
eIND_QUAD = pQUADRA
eZERO = "000"
eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
USE &eARQ INDEX &eIND1,&eIND2,&eIND3
SET FILTER TO CLASSE = STR(pT_PRO,5) .AND. QUADRA = pQUADRA .AND. NUM_IMOVEL = pUNIDADE .AND. PARTE = pM_UNID 
GOTO TOP
PLQUA("01")
PMENSAG("Aguarde Efetuando Desist‡ncia !")
IF EOF()
   PLQUA("01")
   PMSGR( "Aten‡Æo !",3)
   PMENSAG("Nao H  Parcelas !")
ENDIF
SELE 8
GOTO TOP
DO WHILE .NOT. EOF()
   eNUM_PARC   = 0
   eTIPO_PARC  = SPACE(3)
   eDATA_VENC  = CTOD("")
   eVLR_PARC   = 0
   eDATA_PGTO  = CTOD("")
   eVLR_PGTO   = 0
   eOBS        = SPACE(10)
   eORIGEM     = SPACE(3)
   eNUM_PARC   = NUM_PARC
   eTIPO_PARC  = TIPO_PARC
   eDATA_VENC  = DATA_VENC
   eVLR_PARC   = VLR_PARC
   eDATA_PGTO  = DATA_PGTO
   eVLR_PGTO   = VLR_PGTO
   eOBS        = OBS
   eVLR_ORIG   = VLR_ORIGIN
   eORIGEM     = ORIGEM
   eATIVA      = ATIVA
   eCOD_BANCO  = COD_BANCO
   ePERCENTUAL = PERCENTUAL
   ePROTESTO   = PROTESTO
   eCARTEIRA   = CARTEIRA
   eCONTABIL   = CONTABIL
   eMULTA      = MULTA
   eCOMISSAO   = COMISSAO
   ePERID_COMI = PERID_COMI
   eDATA_CONTA = DATA_CONTA
   eCONTA      = CONTA
   eINDICE     = INDICE
   eTAXA_ACUM  = TAXA_ACUM
   eTAXA_MES   = TAXA_MES
   eRESI_MES   = RESI_MES
   eRESI_ACUM  = RESI_ACUM
   eVLR_BASE   = VLR_BASE
   eNUM_BANCO  = NUM_BANCO
   eNUM_AGENC  = NUM_AGENC
   eNUM_CONTA  = NUM_CONTA
   eNUM_CHEQUE = NUM_CHEQUE

   IF eVLR_PGTO <> 0 .AND. DATA_PGTO <> CTOD("")
      SELE 9
      APPEND BLANK
      IF F_BLOQR()
         REPLACE CLASSE      WITH STR(pT_PRO,5),;
                 QUADRA      WITH pQUADRA,;
                 NUM_IMOVEL  WITH pUNIDADE
         REPLACE PARTE       WITH pM_UNID,;
                 NUM_PARC    WITH eNUM_PARC,;
                 ORIGEM      WITH eORIGEM
         REPLACE TIPO_PARC   WITH eTIPO_PARC,;
                 DATA_VENC   WITH eDATA_VENC,;
                 OBS         WITH eOBS
         REPLACE VLR_PARC    WITH eVLR_PARC,;
                 DATA_PGTO   WITH eDATA_PGTO,;
                 VLR_PGTO    WITH eVLR_PGTO
         REPLACE VLR_ORIGIN  WITH eVLR_ORIG,;
                 ORIGEM      WITH eORIGEM,;
                 ATIVA       WITH eATIVA
         REPLACE COD_BANCO   WITH eCOD_BANCO,;
                 PERCENTUAL  WITH ePERCENTUAL,;
                 PROTESTO    WITH ePROTESTO
         REPLACE CARTEIRA    WITH eCARTEIRA,;
                 CONTABIL    WITH eCONTABIL,;
                 MULTA       WITH eMULTA
         REPLACE COMISSAO    WITH eCOMISSAO,;
                 PERID_COMI  WITH ePERID_COMI,;
                 DATA_CONTA  WITH eDATA_CONTA
         REPLACE CONTA       WITH eCONTA,;
                 INDICE      WITH eINDICE,;
                 TAXA_ACUM   WITH eTAXA_ACUM
         REPLACE TAXA_MES    WITH eTAXA_MES,;
                 RESI_MES    WITH eRESI_MES,;
                 RESI_ACUM   WITH eRESI_ACUM
         REPLACE VLR_BASE    WITH eVLR_BASE,;
                 NUM_BANCO   WITH eNUM_BANCO,;
                 NUM_AGENC   WITH eNUM_AGENC
         REPLACE NUM_CONTA   WITH eNUM_CONTA,;
                 NUM_CHEQUE  WITH eNUM_CHEQUE
         UNLOCK
      ENDIF
   ENDIF
   SELE 8
   SKIP
ENDDO
SELE 8
GOTO TOP
IF F_BLOQA()
   DELETE ALL FOR CLASSE = STR(pT_PRO,5) .AND. QUADRA = pQUADRA .AND. NUM_IMOVEL = pUNIDADE .AND. PARTE = pM_UNID 
   UNLOCK
ENDIF
SELE 6
IF F_BLOQR()
   ePARCELAS   = PARCELAS
   eVLR_PARC   = VLR_PARCEL
   ePRI_VENC   = PRI_VENC
   eINTERMEDIA = INTERMEDIA
   eNUM_INTERM = NUM_INTERM
   DELETE  
   UNLOCK
ENDIF
SELE 1
IF pM_UNID = 0 .OR. pM_UNID = 1
   IF F_BLOQR()
      REPLACE PROPRIET_1 WITH 1
      UNLOCK
   ENDIF
ELSE
   IF F_BLOQR()
      REPLACE PROPRIET_2 WITH 1
      UNLOCK
   ENDIF
ENDIF
SELE 7
APPEND BLANK
IF F_BLOQR()
   REPLACE CLASSE      WITH STR(pT_PRO,5),;
           QUADRA      WITH pQUADRA,;
           NUM_IMOVEL  WITH pUNIDADE
   REPLACE PARTE       WITH pM_UNID,;
           NUM_CLIE    WITH eNUM_CLIE,;
           DATA_RENEG  WITH DATE()
   REPLACE PARCELAS    WITH ePARCELAS,;
           VLR_PARCEL  WITH eVLR_PARC,;
           PRI_VENC    WITH ePRI_VENC
   REPLACE INTERMEDIA  WITH eINTERMEDIA,;
           NUM_INTERM  WITH eNUM_INTERM,;
           TIPO_RENEG  WITH "D"
   REPLACE NUM_RENEG   WITH 1
   UNLOCK
   F_LACESSO(sisUSUARIO,"ROTINAS ESPECIAIS","DESISTENCIA IMOVEL : "+ALLTRIM(STR(pT_PRO,5))+"/"+ALLTRIM(pQUADRA)+"/"+ALLTRIM(pUNIDADE)+"/"+ALLTRIM(STR(pM_UNID,1))) 
ENDIF
PLQUA("01")
PMSGR( "Desist‡ncia Concluida !",3)
RETURN 0
*************************  fim  *********************************
