*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REME_COB.PRG
* Data        : 01/01/99
* Objetivo	: GERACAO DO ARQUIVO REMESSA PARA MANUTENá∆o
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REME_COB()
SAVE SCREEN TO TELA03
PTIT( "GERAÄ«O DE ARQUIVO REMESSA")
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
USE BANCOS INDEX BANCOS
GOTO TOP
PLIMPA()
SET COLOR TO W/N,+W/R
eCOD_BANCO = 0
@ 05,02 SAY "Banco   : "
GO TOP
DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
AFIELDS(CAMPO,.T.,LENS)
PIC[1]="@R9999"
PIC[2]="@S50"
CAB[1]="    "
CAB[2]="    "
DBEDIT(5,15,5,75,CAMPO,"F_CONF_COB",PIC,CAB," "," - "," ")
eDRIVE     = DRIVE
eSUBDIR    = SUBDIR_REM
eSUBDIR2   = SUBDIR_ENV
eCORI_REM  = CORI_REM
eDIR       = ALLTRIM(eDRIVE)+":"+ALLTRIM(eSUBDIR)
eDIR2      = ALLTRIM(eDRIVE)+":"+ALLTRIM(eSUBDIR2)
eASSUNTO   = ALLTRIM(ASSUNTO)
eCAIXA_POS = CAIXA_POS
eCOD_CART  = COD_CARTE
eTAM_CAR   = LENNUM(eCOD_CART)
eCOD_CART  = STR(eCOD_CART,eTAM_CAR,0)
eCOD_CART  = ALLTRIM(eCOD_CART)
eCOD_CART  = REPLICATE("0",2-eTAM_CAR)+ALLTRIM(eCOD_CART)
eBANCO_GERADOR = COD_BANCO
eSEQUENCIA = SEQUENCIA
DO CASE
   CASE COD_BANCO = 104
      eASSUNTO   = ALLTRIM(ASSUNTO)
      eCAIXA_POS = ALLTRIM(CAIXA_POS)
   CASE COD_BANCO = 422
      eNUM_INI = NUM_INI
      eNUM_FIM = NUM_FIM
      eNUM_ATU = NUM_ATU
ENDCASE
* Informaá‰es de devoluá∆o e Protesto
ePRAZO_FOL = PRAZO_FOL
ePRAZO_DEV = PRAZO_DEV
ePRAZO_PRO = PRAZO_PRO
eMULTA     = MULTA
eJUROS     = JUROS
eCOM_PERM  = COM_PERM
eTAM_AUX   = LENNUM(ePRAZO_FOL)
ePRAZO_FOL = REPLICATE("0",2-eTAM_AUX)+ALLTRIM(STR(ePRAZO_FOL,eTAM_AUX))
eTAM_AUX   = LENNUM(ePRAZO_DEV)
ePRAZO_DEV = REPLICATE("0",2-eTAM_AUX)+ALLTRIM(STR(ePRAZO_DEV,eTAM_AUX))
eTAM_AUX   = LENNUM(ePRAZO_PRO)
ePRAZO_PRO = REPLICATE("0",2-eTAM_AUX)+ALLTRIM(STR(ePRAZO_PRO,eTAM_AUX))
eTOT_TIT = 0
eVLR_TIT = 0
PLQUA("01")
PPERG ( "Deseja Mensagem de Boleto Autom·tica (S/N) ? ")
IF sisRESP = "S"
   eMSG_AUTO = .T.
ELSE
   eMSG_AUTO = .F.
ENDIF
SELE 8
* Cadastro de Parcelas
SELE 9
USE MSG_BOLE INDEX MSG_PROD,MSG_BOLE
GOTO TOP
SELE 2
* Selecao do produto
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   eCLAS_PRO = 0
   eTIPO_PRO = 0
   @ 06,02 SAY "Produto : "
   GO TOP
   DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1]="@R99999"
   PIC[2]="@R99999"
   PIC[3]="@S40"
   CAB[1]="    "
   CAB[2]="    "
   CAB[3]="    "
   DBEDIT(6,15,6,70,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
   eCLAS_PRO  = CODIGO
   eTIPO_PRO  = TIPO
   eCOD_CONTA = COD_CONTA
   EXIT
ENDDO
* Selecao da rotina de Alteracao conforme o tipo de produto
eQUADRA = SPACE(1)
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
      PPERG ( "Deseja Selecionar "+sisGRUPO1+" (S/N) ? ")
      IF sisRESP = "S"
         @ 07,02 SAY sisGRUPO1+" : "
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
         DBEDIT(7,14,7,75,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
         eQUADRA     = NUM_QUADRA
         eNUM_INICIO = NUM_INICIO
         eNUM_FINAL  = NUM_FINAL
         PLQUA("01")
         PPERG ( "Deseja Selecionar "+sisUNID1+" (S/N) ? ")
         IF sisRESP = "S"
            @ 08,02 SAY sisUNID1+" : "
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
            GOTO TOP
            IF EOF()
               PLQUA("01")
			   PMSGR( "Atená∆o !",3)
			   PMENSAG("N∆o H† "+sisUNID1+" Dispon°veis !")
               PPARA()
               CLOSE DATABASES
               RESTORE SCREEN FROM TELA03
               RETURN nil
            ENDIF
            DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
            AFIELDS(CAMPO,.T.,LENS)
            CAMPO[5]="PROPRIET_1"
            CAMPO[6]="PROPRIET_2"
            LENS[5] =5
            LENS[6] =5
            PIC[1]="@R99999"
            PIC[2]="@R99999"
            PIC[3]="@R99999"
            PIC[4]="@R99999"
            PIC[5]="@R99999"
            PIC[6]="@R99999"
            CAB[1]="     "
            CAB[2]="     "
            CAB[3]="     "
            CAB[4]="     "
            CAB[5]="     "
            CAB[6]="     "
            DBEDIT(8,15,8,69,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
            eNUM_IMOVEL = NUM_IMOVEL
            eSTATUS     = STATUS
            ePROPR_1    = PROPRIET_1
            ePROPR_2    = PROPRIET_2
            ePARTE      = STATUS
            IF ePARTE <> 0
               eOPCAO = 0
               PLQUA("01")
               @ 23,00 SAY "Informe Emiss∆o Para o (1)o. ou (2)o. Meio Lote ? " GET eOPCAO PICT "99999" RANGE 1,2
               READ
               IF eOPCAO = 1
                  ePARTE = 1
               ELSE
                  ePARTE = 2
               ENDIF
            ENDIF
         ENDIF
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
         @ 07,02 SAY sisGRUPO2+"   : "
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
         DBEDIT(7,08,7,76,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
         eQUADRA      = NUM_BLOCO
         eQUANTIDADE = QUANTIDADE
         PLQUA("01")
         PPERG ( "Deseja Selecionar o "+sisUNID2+" (S/N) ? ")
         IF sisRESP = "S"
            @ 08,02 SAY sisUNID2+" : "
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
            GOTO TOP
            IF EOF()
               PLQUA("01")
               PMSGR( "Atená∆o !",3)
	       PMENSAG("N∆o H† "+sisUNID2+" Dispon°veis !")
               PPARA()
               CLOSE DATABASES
               RESTORE SCREEN FROM TELA03
               RETURN nil
            ENDIF
            DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
            AFIELDS(CAMPO,.T.,LENS)
            CAMPO[5]="PROPRIET_1"
            CAMPO[6]="PROPRIET_2"
            LENS[5] =5
            LENS[6] =5
            PIC[1]="@R99999"
            PIC[2]="@R99999"
            PIC[3]="@R99999"
            PIC[4]="@R99999"
            PIC[5]="@R99999"
            PIC[6]="@R99999"
            CAB[1]="     "
            CAB[2]="     "
            CAB[3]="     "
            CAB[4]="     "
            CAB[5]="     "
            CAB[6]="     "
            DBEDIT(8,15,8,69,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
            eNUM_IMOVEL = NUM_IMOVEL
            eSTATUS     = STATUS
            ePROPR_1    = PROPRIET_1
            ePROPR_2    = PROPRIET_2
            ePARTE      = STATUS
         ENDIF
      ENDIF
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   @ 10,02 SAY "Periodo : " GET eDATA_INI
   @ 10,25 SAY "Ate : "     GET eDATA_FIM
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
eARQUIVO    = "RE"+SPACE(6)
DO WHILE .T.
   @ 12,02 SAY "Arquivo : " GET eARQUIVO 
   READ
   IF eARQUIVO = SPACE(8)
      PLQUA("01")
      PMSGR( "Arquivo Invalido !",3)
      LOOP
   ELSE
      eARQ_ENV = eDIR2+ALLTRIM(eARQUIVO)+".ENV"
      eARQUIVO = eDIR+ALLTRIM(eARQUIVO)+"."+ALLTRIM(eCORI_REM)
      EXIT
   ENDIF
ENDDO
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
   GOTO TOP
   IF eNUM_IMOVEL <> SPACE(5)
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO = 0) .AND. ORIGEM <> "B"
      GOTO TOP
   ELSE
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO = 0) .AND. ORIGEM <> "B"
      GOTO TOP
   ENDIF
   SET DEVICE TO SCREEN
   IF EOF()
      PLQUA("01")
      PMSGR( "Atená∆o !",3)
      PMENSAG("N∆o H† Lanáamentos Para Serem Gerados !")
      PPARA()
   ELSE
      PLQUA("01")
      PMSGR( "Atená∆o !",3)
      PMENSAG("Gerando Lanáamentos !")
      SELE 1
      USE CONTAS INDEX CONTAS
      GOTO TOP
      SEEK eCOD_CONTA
      eTAM_BANCO = LENNUM(INT(COD_BANCO))
      eNUM_BANCO = COD_BANCO
      IF COD_BANCO <> eBANCO_GERADOR
         PLQUA("01")
         PMSGR( "Atená∆o !!!",3)
         PMENSAG("C¢digo do Banco Selecionado n∆o coincide com a Banco do Produto, Geraá∆o Cancelada !!!")
         PPARA()
         SET DEVICE TO SCREEN
         CLOSE DATABASES
         RESTORE SCREEN FROM TELA03
         RETURN nil
      ENDIF
      SELE 7
      USE BANCOS INDEX BANCOS
      GOTO TOP
      SEEK eNUM_BANCO
      eNUM_ARQ    = ARQUIVO + 1
      eTAM_NUM_ARQ= LENNUM(INT(eNUM_ARQ))
      eNUM_ARQ    = REPLICATE("0",5-eTAM_NUM_ARQ)+ALLTRIM(STR(eNUM_ARQ,10))
      eNOME_BANCO = SUBSTR(NOME_BANCO,1,15)      
      eNUM_BANCO  = REPLICATE("0",3-eTAM_BANCO)+ALLTRIM(STR(eNUM_BANCO,3))
      SELE 1
      USE CONTAS INDEX CONTAS
      GOTO TOP
      SEEK eCOD_CONTA
      DO CASE
         CASE eNUM_BANCO = "104"  
            IF eASSUNTO = "SIGCB"
            	    * SIGCB
		    eTAM_COD_AG = LEN(ALLTRIM(COD_AGENCI))
		    eCOD_AGENCI = REPLICATE("0",4-eTAM_COD_AG)+ALLTRIM(COD_AGENCI)
		    eTAM_COD_CC = LEN(COD_CC)
		    IF (eTAM_COD_CC <= 6)
		    	eCOD_CC     = REPLICATE("0",6-eTAM_COD_CC)+ALLTRIM(COD_CC)
		    ELSE
		    	eCOD_CC     = SUBSTR(COD_CC,1,6)		    
		    ENDIF
		    eTAM_COD_CO = LEN(ALLTRIM(COMPLEMENT))
		    eCOMPLEM    = REPLICATE("0",6-eTAM_COD_CO)+ALLTRIM(COMPLEMENT)   
            ELSE
            	    * SICOB          
		    eTAM_COD_AG = LEN(ALLTRIM(COD_AGENCI))
		    eCOD_AGENCI = REPLICATE("0",4-eTAM_COD_AG)+ALLTRIM(COD_AGENCI)
		    eTAM_COD_CC = LEN(COD_CC)
		    eCOD_CC     = REPLICATE("0",12-eTAM_COD_CC)+ALLTRIM(COD_CC)
		    eTAM_COD_CO = LEN(COMPLEMENT)
		    eCOMPLEM    = REPLICATE("0",20-eTAM_COD_CO)+ALLTRIM(COMPLEMENT)               
            ENDIF
         CASE eNUM_BANCO = "341"
            eTAM_COD_AG = LEN(ALLTRIM(COD_AGENCI))
            eCOD_AGENCI = REPLICATE("0",4-eTAM_COD_AG)+ALLTRIM(COD_AGENCI)
            eTAM_COD_CC = LEN(COD_CC)
            eCOD_CC     = REPLICATE("0",9-eTAM_COD_CC)+ALLTRIM(COD_CC)
            eTAM_COD_CO = LEN(COMPLEMENT)
            eCOMPLEM    = REPLICATE("0",20-eTAM_COD_CO)+ALLTRIM(COMPLEMENT)
         CASE eNUM_BANCO = "422"
            eTAM_COD_AG = LEN(ALLTRIM(COD_AGENCI))
            eCOD_AGENCI = REPLICATE("0",5-eTAM_COD_AG)+ALLTRIM(COD_AGENCI)
            eTAM_COD_CC = LEN(ALLTRIM(COD_CC))
            eCOD_CC     = REPLICATE("0",9-eTAM_COD_CC)+ALLTRIM(COD_CC)
            eTAM_COD_CO = LEN(COMPLEMENT)
            eCOMPLEM    = REPLICATE("0",20-eTAM_COD_CO)+ALLTRIM(COMPLEMENT)
         CASE eNUM_BANCO = "424"
            eTAM_COD_AG = LEN(ALLTRIM(COD_AGENCI))
            eCOD_AGENCI = REPLICATE("0",3-eTAM_COD_AG)+ALLTRIM(COD_AGENCI)
            eTAM_COD_CC = LEN(COD_CC)
            eCOD_CC     = REPLICATE("0",8-eTAM_COD_CC)+ALLTRIM(COD_CC)
            eTAM_COD_CO = LEN(COMPLEMENT)
            eCOMPLEM    = REPLICATE("0",20-eTAM_COD_CO)+ALLTRIM(COMPLEMENT)
         CASE eNUM_BANCO = "641"
            eTAM_COD_AG = LEN(ALLTRIM(COD_AGENCI))
            eCOD_AGENCI = REPLICATE("0",4-eTAM_COD_AG)+ALLTRIM(COD_AGENCI)
            eTAM_COD_CC = LEN(COD_CC)
            eCOD_CC     = REPLICATE("0",10-eTAM_COD_CC)+ALLTRIM(COD_CC)
            eTAM_COD_CO = LEN(COMPLEMENT)
            eCOMPLEM    = REPLICATE("0",20-eTAM_COD_CO)+ALLTRIM(COMPLEMENT)
         OTHERWISE
            eTAM_COD_AG = LEN(ALLTRIM(COD_AGENCI))
            eCOD_AGENCI = REPLICATE("0",4-eTAM_COD_AG)+ALLTRIM(COD_AGENCI)
            eTAM_COD_CC = LEN(COD_CC)
            eCOD_CC     = REPLICATE("0",12-eTAM_COD_CC)+ALLTRIM(COD_CC)
            eTAM_COD_CO = LEN(COMPLEMENT)
            eCOMPLEM    = REPLICATE("0",20-eTAM_COD_CO)+ALLTRIM(COMPLEMENT)
      ENDCASE
      eOPERACAO   = "000"
      SELE 9
      USE
      USE MSG_BOLE INDEX MSG_BOLE
      GOTO TOP
      SEEK eTIPO_PRO
      eMSG     = SUBSTR(LINHA_1,1,40)
      eMSG_2   = SUBSTR(LINHA_2,1,40)
      eMSG_3   = SUBSTR(LINHA_3,1,40)
      eMSG_4   = SUBSTR(LINHA_4,1,40)
      eMSG_5   = SUBSTR(LINHA_5,1,40)
      eEMPRESA = IIF(LEN(ALLTRIM(SUBSTR(sisTIT1,1,30))) < 30,SUBSTR(sisTIT1,1,30)+REPLICATE(" ",30 - LEN(ALLTRIM(SUBSTR(sisTIT1,1,30)))),SUBSTR(sisTIT1,1,30))
      * Preparaá∆o dos Dados para o arquivo
      SET PRINT ON
      SET PRINTER TO &eARQUIVO
      * Registro Header
      eNUM_SEQ = 2

      HEADERINI()

      SELE 8
      GOTO TOP
      DO WHILE .NOT. EOF()
         eNUM_IMOVEL = NUM_IMOVEL
         eQUADRA     = QUADRA
         ePARTE      = PARTE      
         SELE 6
         USE
         USE RENEGOCI INDEX RENEGOCI                            
         GOTO TOP
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE
         GOTO TOP
         IF EOF()
            SELE 6
            USE CAD_VEND INDEX CAD_VEND,DATA_VEN
            GOTO TOP
            SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
            eQUANT_PARC = PARCELAS
         ELSE
            GOTO BOTTOM
            eQUANT_PARC = PARCELAS
         ENDIF
         SELE 8
         eVAZIO  = "00"
         eINDENT = SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(STR(eCLAS_PRO,5)))))+ALLTRIM(STR(eCLAS_PRO,5))+SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(QUADRA))))+ALLTRIM(QUADRA)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(NUM_IMOVEL))))+ALLTRIM(NUM_IMOVEL)+ALLTRIM(STR(PARTE,1))
         IF LEN(eINDENT) > 8
            eINDENT = SUBSTR(eINDENT,1,8)
         ENDIF
         eQUADRA     = QUADRA
         eNUM_IMOVEL = NUM_IMOVEL
         ePARTE      = PARTE
         SELE 6
         USE CAD_VEND INDEX CAD_VEND,DATA_VEN
         GOTO TOP
         SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
         ePROPR = NUM_CLIE
         SELE 5
         GOTO TOP
         SEEK ePROPR
         eCLIENTE = CLIENTE
         eSEXO    = SEXO
         eCPF_CGC = CPF_CGC         
         IF OPCAO = "R"
            eENDERECO = END_R
            eBAIRRO   = BAIRRO_R
            eCEP      = CEP_R
            eCIDADE   = CIDADE_R
            eESTADO   = ESTADO_R
         ELSE
            eENDERECO = END_C
            eBAIRRO   = BAIRRO_C
            eCEP      = CEP_C
            eCIDADE   = CIDADE_C
            eESTADO   = ESTADO_C
         ENDIF
         SELE 8
         eCOD_BANCO  = ALLTRIM(COD_BANCO)
         * Multas e Comissoes
         eMULTA     = MULTA
         eJUROS     = COMISSAOeSEQUENCIA
         * Numero da Parcela
         eNUM_PARC = ALLTRIM(NUM_PARC)
         eTAM_PARC = LEN(eNUM_PARC)
         eNUM_PARC = REPLICATE("0",5-eTAM_PARC)+ALLTRIM(eNUM_PARC)
         * Alteraá∆o Solida para status = numero parcela, sem repetiá∆o
         IF sisCGC_CPF = "02852392000142".OR. sisCGC_CPF = "06983112000196"
            eINDENT = SUBSTR(eINDENT,1,7)+SUBSTR(eNUM_PARC,5,1)
         ENDIF
         * Registro de Lanáamentos
         eESP_IDENT = SPACE(25-LEN(eINDENT))
         * Valor da Parcela
         eNUM_PARCE = NUM_PARC
         eVLR_PARCE = INT(ROUND((VLR_PARC*100),0))
         eTAM_PARCE = LENNUM(eVLR_PARCE)
         eVLR_PARCE = STR(eVLR_PARCE,eTAM_PARCE,0)
         eVLR_PARCE = ALLTRIM(eVLR_PARCE)
         eVLR_PARCE = REPLICATE("0",13-eTAM_PARCE)+ALLTRIM(eVLR_PARCE)
         eTOT_TIT   = eTOT_TIT + 1
         eVLR_TIT   = eVLR_TIT + VLR_PARC
         * Data de Vencimento
         eDT_VENC   = SUBSTR(DTOC(DATA_VENC),1,2)+SUBSTR(DTOC(DATA_VENC),4,2)+SUBSTR(DTOC(DATA_VENC),9,2)
         eDT_EMIS   = SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,2)
         eDT_MULT   = DATA_VENC + 5
         eDT_MULT   = SUBSTR(DTOC(eDT_MULT),1,2)+SUBSTR(DTOC(eDT_MULT),4,2)+SUBSTR(DTOC(eDT_MULT),9,2)
         * Identificacao Sacado
         IF eSEXO = "F" .OR. eSEXO = "M" 
            eTIPO_SAC = "01"
         ELSE
            eTIPO_SAC = "02" 
         ENDIF
         * CGC ou CPF Sacado
         eTAM_CGC_CPF = LEN(eCPF_CGC)
         eAUX_CGC     = SPACE(14)
         eAUX = 1
         DO WHILE eAUX <= eTAM_CGC_CPF
            IF SUBSTR(eCPF_CGC,eAUX,1) = "." .OR. SUBSTR(eCPF_CGC,eAUX,1) = "-" .OR. SUBSTR(eCPF_CGC,eAUX,1) = "/" .OR. SUBSTR(eCPF_CGC,eAUX,1) = " "
               eAUX = eAUX + 1
            ELSE
               eAUX_CGC = ALLTRIM(eAUX_CGC) + SUBSTR(eCPF_CGC,eAUX,1)
               eAUX = eAUX + 1
            ENDIF
         ENDDO
         eCPF_CGC = ALLTRIM(eAUX_CGC)
         * CEP do Sacado
         eTAM_CEP = LEN(ALLTRIM(eCEP))
         IF eTAM_CEP <> 0
            eCEP = REPLICATE("0",8-eTAM_CEP)+ALLTRIM(eCEP)
         ELSE 
            eCEP = REPLICATE("0",8)
         ENDIF
         * Numero Sequencial do Registro
         eTAM_SEQ = LENNUM(eNUM_SEQ)
         eREG_SEQ = REPLICATE("0",6-eTAM_SEQ)+STR(eNUM_SEQ,eTAM_SEQ,0)
         SELE 8
         DO REGISTRO
         SKIP
      ENDDO
      * Registro Header Final
      * Numero Sequencial do Registro

      HEADERFIM()

      SET PRINT OFF
      SELE 7
      USE BANCOS INDEX BANCOS
      GOTO TOP
      SEEK VAL(eNUM_BANCO)
      IF F_BLOQR()
         REPLACE ARQUIVO WITH ARQUIVO + 1
         REPLACE SEQUENCIA WITH eSEQUENCIA
         UNLOCK
      ENDIF
   ENDIF
ELSE
   IF eTIPO_PRO = 1
      PLQUA("01")
      PMSGR( "Atená∆o !",3)
      PMENSAG("Gerando Lanáamentos !")
      SELE 1
      USE CONTAS INDEX CONTAS
      GOTO TOP
      SEEK eCOD_CONTA
      eTAM_BANCO = LENNUM(INT(COD_BANCO))
      eNUM_BANCO = COD_BANCO
      IF COD_BANCO <> eBANCO_GERADOR
         PLQUA("01")
         PMSGR( "Atená∆o !!!",3)
         PMENSAG("C¢digo da Conta n∆o coincide com a Conta Corente do Produto, Geraá∆o Cancelada !!!")
         PPARA()
         SET DEVICE TO SCREEN
         CLOSE DATABASES
         RESTORE SCREEN FROM TELA03
         RETURN nil
      ENDIF
      SELE 7
      USE BANCOS INDEX BANCOS
      GOTO TOP
      SEEK eNUM_BANCO
      eNUM_ARQ    = ARQUIVO + 1
      eTAM_NUM_ARQ= LENNUM(INT(eNUM_ARQ))
      eNUM_ARQ    = REPLICATE("0",5-eTAM_NUM_ARQ)+ALLTRIM(STR(eNUM_ARQ,10))
      eNOME_BANCO = SUBSTR(NOME_BANCO,1,15)      
      eNUM_BANCO  = REPLICATE("0",3-eTAM_BANCO)+ALLTRIM(STR(eNUM_BANCO,3))
      SELE 1
      USE CONTAS INDEX CONTAS
      GOTO TOP
      SEEK eCOD_CONTA
      DO CASE
         CASE eNUM_BANCO = "104"  
            IF eASSUNTO = "SIGCB"
            	    * SIGCB
		    eTAM_COD_AG = LEN(ALLTRIM(COD_AGENCI))
		    eCOD_AGENCI = REPLICATE("0",4-eTAM_COD_AG)+ALLTRIM(COD_AGENCI)
		    eTAM_COD_CC = LEN(COD_CC)
		    IF (eTAM_COD_CC <= 6)
		    	eCOD_CC     = REPLICATE("0",6-eTAM_COD_CC)+ALLTRIM(COD_CC)
		    ELSE
		    	eCOD_CC     = SUBSTR(COD_CC,1,6)		    
		    ENDIF
		    eTAM_COD_CO = LEN(ALLTRIM(COMPLEMENT))
		    eCOMPLEM    = REPLICATE("0",6-eTAM_COD_CO)+ALLTRIM(COMPLEMENT)   
            ELSE
            	    * SICOB          
		    eTAM_COD_AG = LEN(ALLTRIM(COD_AGENCI))
		    eCOD_AGENCI = REPLICATE("0",4-eTAM_COD_AG)+ALLTRIM(COD_AGENCI)
		    eTAM_COD_CC = LEN(COD_CC)
		    eCOD_CC     = REPLICATE("0",12-eTAM_COD_CC)+ALLTRIM(COD_CC)
		    eTAM_COD_CO = LEN(COMPLEMENT)
		    eCOMPLEM    = REPLICATE("0",20-eTAM_COD_CO)+ALLTRIM(COMPLEMENT)               
            ENDIF      
         CASE eNUM_BANCO = "341"
            eTAM_COD_AG = LEN(ALLTRIM(COD_AGENCI))
            eCOD_AGENCI = REPLICATE("0",4-eTAM_COD_AG)+ALLTRIM(COD_AGENCI)
            eTAM_COD_CC = LEN(COD_CC)
            eCOD_CC     = REPLICATE("0",9-eTAM_COD_CC)+ALLTRIM(COD_CC)
            eTAM_COD_CO = LEN(COMPLEMENT)
            eCOMPLEM    = REPLICATE("0",20-eTAM_COD_CO)+ALLTRIM(COMPLEMENT)
         CASE eNUM_BANCO = "422"
            eTAM_COD_AG = LEN(ALLTRIM(COD_AGENCI))
            eCOD_AGENCI = REPLICATE("0",5-eTAM_COD_AG)+ALLTRIM(COD_AGENCI)
            eTAM_COD_CC = LEN(ALLTRIM(COD_CC))
            eCOD_CC     = REPLICATE("0",9-eTAM_COD_CC)+ALLTRIM(COD_CC)
            eTAM_COD_CO = LEN(COMPLEMENT)
            eCOMPLEM    = REPLICATE("0",20-eTAM_COD_CO)+ALLTRIM(COMPLEMENT)
         CASE eNUM_BANCO = "424"
            eTAM_COD_AG = LEN(ALLTRIM(COD_AGENCI))
            eCOD_AGENCI = REPLICATE("0",3-eTAM_COD_AG)+ALLTRIM(COD_AGENCI)
            eTAM_COD_CC = LEN(COD_CC)
            eCOD_CC     = REPLICATE("0",8-eTAM_COD_CC)+ALLTRIM(COD_CC)
            eTAM_COD_CO = LEN(COMPLEMENT)
            eCOMPLEM    = REPLICATE("0",20-eTAM_COD_CO)+ALLTRIM(COMPLEMENT)
         CASE eNUM_BANCO = "641"
            eTAM_COD_AG = LEN(ALLTRIM(COD_AGENCI))
            eCOD_AGENCI = REPLICATE("0",4-eTAM_COD_AG)+ALLTRIM(COD_AGENCI)
            eTAM_COD_CC = LEN(COD_CC)
            eCOD_CC     = REPLICATE("0",10-eTAM_COD_CC)+ALLTRIM(COD_CC)
            eTAM_COD_CO = LEN(COMPLEMENT)
            eCOMPLEM    = REPLICATE("0",20-eTAM_COD_CO)+ALLTRIM(COMPLEMENT)
         OTHERWISE
            eTAM_COD_AG = LEN(ALLTRIM(COD_AGENCI))
            eCOD_AGENCI = REPLICATE("0",4-eTAM_COD_AG)+ALLTRIM(COD_AGENCI)
            eTAM_COD_CC = LEN(COD_CC)
            eCOD_CC     = REPLICATE("0",12-eTAM_COD_CC)+ALLTRIM(COD_CC)
            eTAM_COD_CO = LEN(COMPLEMENT)
            eCOMPLEM    = REPLICATE("0",20-eTAM_COD_CO)+ALLTRIM(COMPLEMENT)
      ENDCASE
      eOPERACAO   = "000"
      SELE 9
      USE
      USE MSG_BOLE INDEX MSG_BOLE
      GOTO TOP
      SEEK eTIPO_PRO
      eMSG     = SUBSTR(LINHA_1,1,40)
      eMSG_2   = SUBSTR(LINHA_2,1,40)
      eMSG_3   = SUBSTR(LINHA_3,1,40)
      eMSG_4   = SUBSTR(LINHA_4,1,40)
      eMSG_5   = SUBSTR(LINHA_5,1,40)
      eEMPRESA = IIF(LEN(ALLTRIM(SUBSTR(sisTIT1,1,30))) < 30,SUBSTR(sisTIT1,1,30)+REPLICATE(" ",30 - LEN(ALLTRIM(SUBSTR(sisTIT1,1,30)))),SUBSTR(sisTIT1,1,30))
      SELE 3
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      SET PRINT ON
      SET PRINTER TO &eARQUIVO
      * Registro Header
      eNUM_SEQ = 2

      HEADERINI()

      DO WHILE .NOT. EOF()
         eIND_CLAS = NUM_LOTEAM
         eIND_QUAD = NUM_QUADRA
         SELE 8
         eZERO = "000"
         eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         USE &eARQ INDEX &eIND1,&eIND2,&eIND3
         GOTO TOP
         SET FILTER TO CLASSE = eIND_CLAS .AND. QUADRA = eIND_QUAD .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO = 0) .AND. ORIGEM <> "B"
         GOTO TOP
         DO WHILE .NOT. EOF()
            eNUM_IMOVEL = NUM_IMOVEL
            eQUADRA     = QUADRA
            ePARTE      = PARTE      
            SELE 6
            GOTO TOP
            SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
            IF FOUND()
               eQUANT_PARC = PARCELAS
            ELSE
               eQUANT_PARC = 0
            ENDIF
            SELE 8
            eVAZIO  = "00"
            eINDENT = SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(STR(eCLAS_PRO,5)))))+ALLTRIM(STR(eCLAS_PRO,5))+SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(QUADRA))))+ALLTRIM(QUADRA)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(NUM_IMOVEL))))+ALLTRIM(NUM_IMOVEL)+ALLTRIM(STR(PARTE,1))
            IF LEN(eINDENT) > 8
               eINDENT = SUBSTR(eINDENT,1,8)
            ENDIF
            SELE 6
            USE CAD_VEND INDEX CAD_VEND,DATA_VEN
            GOTO TOP
            SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
            ePROPR = NUM_CLIE
            SELE 5
            GOTO TOP
            SEEK ePROPR
            eSEXO    = SEXO
            eCPF_CGC     = CPF_CGC            
            eCLIENTE = CLIENTE
            IF OPCAO = "R"
               eENDERECO = END_R
               eBAIRRO   = BAIRRO_R
               eCEP      = CEP_R
               eCIDADE   = CIDADE_R
               eESTADO   = ESTADO_R
            ELSE
               eENDERECO = END_C
               eBAIRRO   = BAIRRO_C
               eCEP      = CEP_C
               eCIDADE   = CIDADE_C
               eESTADO   = ESTADO_C
            ENDIF
            SELE 8
            eCOD_BANCO  = ALLTRIM(COD_BANCO)
            * Numero da Parcela
            eNUM_PARC = ALLTRIM(NUM_PARC)
            eTAM_PARC = LEN(eNUM_PARC)
            eNUM_PARC = REPLICATE("0",5-eTAM_PARC)+ALLTRIM(eNUM_PARC)
            * Alteraá∆o Solida para status = numero parcela, sem repetiá∆o
            IF sisCGC_CPF = "02852392000142".OR. sisCGC_CPF = "06983112000196"
               eINDENT = SUBSTR(eINDENT,1,7)+SUBSTR(eNUM_PARC,5,1)
            ENDIF
            * Registro de Lanáamentos
            eESP_IDENT = SPACE(25-LEN(eINDENT))
            * Valor da Parcela
            eVLR_PARCE = INT(ROUND((VLR_PARC*100),0))
            eTAM_PARCE = LENNUM(eVLR_PARCE)
            eVLR_PARCE = STR(eVLR_PARCE,eTAM_PARCE,0)
            eVLR_PARCE = ALLTRIM(eVLR_PARCE)
            eVLR_PARCE = REPLICATE("0",13-eTAM_PARCE)+ALLTRIM(eVLR_PARCE)
            eTOT_TIT   = eTOT_TIT + 1
            eVLR_TIT   = eVLR_TIT + VLR_PARC
            * Data de Vencimento
            eDT_VENC   = SUBSTR(DTOC(DATA_VENC),1,2)+SUBSTR(DTOC(DATA_VENC),4,2)+SUBSTR(DTOC(DATA_VENC),9,2)
            eDT_EMIS   = SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,2)
            eDT_MULT   = DATA_VENC + 5
            eDT_MULT   = SUBSTR(DTOC(DATA_VENC),1,2)+SUBSTR(DTOC(DATA_VENC),4,2)+SUBSTR(DTOC(DATA_VENC),9,2)
            * Identificacao Sacado
            IF eSEXO = "F" .OR. eSEXO = "M" .OR. eSEXO = SPACE(1)
               eTIPO_SAC = "01"
            ELSE
               eTIPO_SAC = "02" 
            ENDIF
            * CGC ou CPF Sacado
            eTAM_CGC_CPF = LEN(eCPF_CGC)
            eAUX_CGC     = SPACE(14)
            eAUX = 1
            DO WHILE eAUX <= eTAM_CGC_CPF
               IF SUBSTR(eCPF_CGC,eAUX,1) = "." .OR. SUBSTR(eCPF_CGC,eAUX,1) = "-" .OR. SUBSTR(eCPF_CGC,eAUX,1) = "/" .OR. SUBSTR(eCPF_CGC,eAUX,1) = " "
                  eAUX = eAUX + 1
               ELSE
                  eAUX_CGC = ALLTRIM(eAUX_CGC) + SUBSTR(eCPF_CGC,eAUX,1)
                  eAUX = eAUX + 1
               ENDIF
            ENDDO
            eCPF_CGC = ALLTRIM(eAUX_CGC)
            * CEP do Sacado
            eTAM_CEP = LEN(ALLTRIM(eCEP))
            IF eTAM_CEP <> 0
               eCEP = REPLICATE("0",8-eTAM_CEP)+ALLTRIM(eCEP)
            ELSE 
               eCEP = REPLICATE("0",8)
            ENDIF
            * Numero Sequencial do Registro
            eTAM_SEQ = LENNUM(eNUM_SEQ)
            eREG_SEQ = REPLICATE("0",6-eTAM_SEQ)+STR(eNUM_SEQ,eTAM_SEQ,0)
            SELE 8
            DO REGISTRO
            SKIP
         ENDDO
         SELE 3
         SKIP
      ENDDO
      * Registro Header Final
      * Numero Sequencial do Registro

      HEADERFIM()

      SET PRINT OFF
      SELE 7
      USE BANCOS INDEX BANCOS
      GOTO TOP
      SEEK VAL(eNUM_BANCO)
      IF F_BLOQR()
         REPLACE ARQUIVO WITH ARQUIVO + 1
         REPLACE SEQUENCIA WITH eSEQUENCIA
         UNLOCK
      ENDIF
   ELSE
      PLQUA("01")
      PMSGR( "Atená∆o !",3)
      PMENSAG("Gerando Lanáamentos !")
      SELE 1
      USE CONTAS INDEX CONTAS
      GOTO TOP
      SEEK eCOD_CONTA
      eTAM_BANCO = LENNUM(INT(COD_BANCO))
      eNUM_BANCO = COD_BANCO
      SELE 7
      USE BANCOS INDEX BANCOS
      GOTO TOP
      SEEK eNUM_BANCO
      eNUM_ARQ    = ARQUIVO + 1
      eTAM_NUM_ARQ= LENNUM(INT(eNUM_ARQ))
      eNUM_ARQ    = REPLICATE("0",5-eTAM_NUM_ARQ)+ALLTRIM(STR(eNUM_ARQ,10))
      eNOME_BANCO = SUBSTR(NOME_BANCO,1,15)      
      eNUM_BANCO  = REPLICATE("0",3-eTAM_BANCO)+ALLTRIM(STR(eNUM_BANCO,3))
      SELE 1
      USE CONTAS INDEX CONTAS
      GOTO TOP
      SEEK eCOD_CONTA
      DO CASE
         CASE eNUM_BANCO = "104"  
            IF eASSUNTO = "SIGCB"
            	    * SIGCB
		    eTAM_COD_AG = LEN(ALLTRIM(COD_AGENCI))
		    eCOD_AGENCI = REPLICATE("0",4-eTAM_COD_AG)+ALLTRIM(COD_AGENCI)
		    eTAM_COD_CC = LEN(COD_CC)
		    IF (eTAM_COD_CC <= 6)
		    	eCOD_CC     = REPLICATE("0",6-eTAM_COD_CC)+ALLTRIM(COD_CC)
		    ELSE
		    	eCOD_CC     = SUBSTR(COD_CC,1,6)		    
		    ENDIF
		    eTAM_COD_CO = LEN(ALLTRIM(COMPLEMENT))
		    eCOMPLEM    = REPLICATE("0",6-eTAM_COD_CO)+ALLTRIM(COMPLEMENT)   
            ELSE
            	    * SICOB          
		    eTAM_COD_AG = LEN(ALLTRIM(COD_AGENCI))
		    eCOD_AGENCI = REPLICATE("0",4-eTAM_COD_AG)+ALLTRIM(COD_AGENCI)
		    eTAM_COD_CC = LEN(COD_CC)
		    eCOD_CC     = REPLICATE("0",12-eTAM_COD_CC)+ALLTRIM(COD_CC)
		    eTAM_COD_CO = LEN(COMPLEMENT)
		    eCOMPLEM    = REPLICATE("0",20-eTAM_COD_CO)+ALLTRIM(COMPLEMENT)               
            ENDIF      
         CASE eNUM_BANCO = "341"
            eTAM_COD_AG = LEN(ALLTRIM(COD_AGENCI))
            eCOD_AGENCI = REPLICATE("0",4-eTAM_COD_AG)+ALLTRIM(COD_AGENCI)
            eTAM_COD_CC = LEN(COD_CC)
            eCOD_CC     = REPLICATE("0",9-eTAM_COD_CC)+ALLTRIM(COD_CC)
            eTAM_COD_CO = LEN(COMPLEMENT)
            eCOMPLEM    = REPLICATE("0",20-eTAM_COD_CO)+ALLTRIM(COMPLEMENT)
         CASE eNUM_BANCO = "422"
            eTAM_COD_AG = LEN(ALLTRIM(COD_AGENCI))
            eCOD_AGENCI = REPLICATE("0",5-eTAM_COD_AG)+ALLTRIM(COD_AGENCI)
            eTAM_COD_CC = LEN(ALLTRIM(COD_CC))
            eCOD_CC     = REPLICATE("0",9-eTAM_COD_CC)+ALLTRIM(COD_CC)
            eTAM_COD_CO = LEN(COMPLEMENT)
            eCOMPLEM    = REPLICATE("0",20-eTAM_COD_CO)+ALLTRIM(COMPLEMENT)
         CASE eNUM_BANCO = "424"
            eTAM_COD_AG = LEN(ALLTRIM(COD_AGENCI))
            eCOD_AGENCI = REPLICATE("0",3-eTAM_COD_AG)+ALLTRIM(COD_AGENCI)
            eTAM_COD_CC = LEN(COD_CC)
            eCOD_CC     = REPLICATE("0",8-eTAM_COD_CC)+ALLTRIM(COD_CC)
            eTAM_COD_CO = LEN(COMPLEMENT)
            eCOMPLEM    = REPLICATE("0",20-eTAM_COD_CO)+ALLTRIM(COMPLEMENT)
         CASE eNUM_BANCO = "641"
            eTAM_COD_AG = LEN(ALLTRIM(COD_AGENCI))
            eCOD_AGENCI = REPLICATE("0",4-eTAM_COD_AG)+ALLTRIM(COD_AGENCI)
            eTAM_COD_CC = LEN(COD_CC)
            eCOD_CC     = REPLICATE("0",10-eTAM_COD_CC)+ALLTRIM(COD_CC)
            eTAM_COD_CO = LEN(COMPLEMENT)
            eCOMPLEM    = REPLICATE("0",20-eTAM_COD_CO)+ALLTRIM(COMPLEMENT)
         OTHERWISE
            eTAM_COD_AG = LEN(ALLTRIM(COD_AGENCI))
            eCOD_AGENCI = REPLICATE("0",4-eTAM_COD_AG)+ALLTRIM(COD_AGENCI)
            eTAM_COD_CC = LEN(COD_CC)
            eCOD_CC     = REPLICATE("0",12-eTAM_COD_CC)+ALLTRIM(COD_CC)
            eTAM_COD_CO = LEN(COMPLEMENT)
            eCOMPLEM    = REPLICATE("0",20-eTAM_COD_CO)+ALLTRIM(COMPLEMENT)
      ENDCASE
      eOPERACAO   = "000"
      SELE 9
      USE
      USE MSG_BOLE INDEX MSG_BOLE
      GOTO TOP
      SEEK eTIPO_PRO
      eMSG     = SUBSTR(LINHA_1,1,40)
      eMSG_2   = SUBSTR(LINHA_2,1,40)
      eMSG_3   = SUBSTR(LINHA_3,1,40)
      eMSG_4   = SUBSTR(LINHA_4,1,40)
      eMSG_5   = SUBSTR(LINHA_5,1,40)
      eEMPRESA = IIF(LEN(ALLTRIM(SUBSTR(sisTIT1,1,30))) < 30,SUBSTR(sisTIT1,1,30)+REPLICATE(" ",30 - LEN(ALLTRIM(SUBSTR(sisTIT1,1,30)))),SUBSTR(sisTIT1,1,30))
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      SET PRINT ON
      SET PRINTER TO &eARQUIVO
      * Registro Header
      eNUM_SEQ = 2

      HEADERINI()

      DO WHILE .NOT. EOF()
         eIND_CLAS = NUM_LOTEAM
         eIND_QUAD = NUM_BLOCO
         SELE 8
         eZERO = "000"
         eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         USE &eARQ INDEX &eIND1,&eIND2,&eIND3
         GOTO TOP
         SET FILTER TO CLASSE = eIND_CLAS .AND. QUADRA = eIND_QUAD .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO = 0) .AND. ORIGEM <> "B"
         GOTO TOP
         DO WHILE .NOT. EOF()
            eNUM_IMOVEL = NUM_IMOVEL
            eQUADRA     = QUADRA
            ePARTE      = PARTE      
            SELE 6
            GOTO TOP
            SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
            IF FOUND()
               eQUANT_PARC = PARCELAS
            ELSE
               eQUANT_PARC = 0
            ENDIF
            SELE 8
            eVAZIO  = "00"
            eINDENT = SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(STR(eCLAS_PRO,5)))))+ALLTRIM(STR(eCLAS_PRO,5))+SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(QUADRA))))+ALLTRIM(QUADRA)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(NUM_IMOVEL))))+ALLTRIM(NUM_IMOVEL)+ALLTRIM(STR(PARTE,1))
            IF LEN(eINDENT) > 8
               eINDENT = SUBSTR(eINDENT,1,8)
            ENDIF
            SELE 6
            IF ePARTE = 0 .OR. ePARTE = 1
               ePROPR_1 = NUM_CLIE
               SELE 5
               GOTO TOP
               SEEK ePROPR_1
            ELSE
               ePROPR_2 = NUM_CLIE
               SELE 5
               GOTO TOP
               SEEK ePROPR_2
            ENDIF
            eSEXO    = SEXO
            eCPF_CGC     = CPF_CGC
            eCLIENTE = CLIENTE
            IF OPCAO = "R"
               eENDERECO = END_R
               eBAIRRO   = BAIRRO_R
               eCEP      = CEP_R
               eCIDADE   = CIDADE_R
               eESTADO   = ESTADO_R
            ELSE
               eENDERECO = END_C
               eBAIRRO   = BAIRRO_C
               eCEP      = CEP_C
               eCIDADE   = CIDADE_C
               eESTADO   = ESTADO_C
            ENDIF
            SELE 8
            eCOD_BANCO  = ALLTRIM(COD_BANCO)
            * Numero da Parcela
            eNUM_PARC = ALLTRIM(NUM_PARC)
            eTAM_PARC = LEN(eNUM_PARC)
            eNUM_PARC = REPLICATE("0",5-eTAM_PARC)+ALLTRIM(eNUM_PARC)
            * Alteraá∆o Solida para status = numero parcela, sem repetiá∆o
            IF sisCGC_CPF = "02852392000142".OR. sisCGC_CPF = "06983112000196"
               eINDENT = SUBSTR(eINDENT,1,7)+SUBSTR(eNUM_PARC,5,1)
            ENDIF
            * Registro de Lanáamentos
            eESP_IDENT = SPACE(25-LEN(eINDENT))
            * Valor da Parcela
            eVLR_PARCE = INT(ROUND((VLR_PARC*100),0))
            eTAM_PARCE = LENNUM(eVLR_PARCE)
            eVLR_PARCE = STR(eVLR_PARCE,eTAM_PARCE,0)
            eVLR_PARCE = ALLTRIM(eVLR_PARCE)
            eVLR_PARCE = REPLICATE("0",13-eTAM_PARCE)+ALLTRIM(eVLR_PARCE)
            eTOT_TIT   = eTOT_TIT + 1
            eVLR_TIT   = eVLR_TIT + VLR_PARC
            * Data de Vencimento
            eDT_VENC   = SUBSTR(DTOC(DATA_VENC),1,2)+SUBSTR(DTOC(DATA_VENC),4,2)+SUBSTR(DTOC(DATA_VENC),9,2)
            eDT_EMIS   = SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,2)
            eDT_MULT   = DATA_VENC + 5
            eDT_MULT   = SUBSTR(DTOC(DATA_VENC),1,2)+SUBSTR(DTOC(DATA_VENC),4,2)+SUBSTR(DTOC(DATA_VENC),9,2)
            * Identificacao Sacado
            IF eSEXO = "F" .OR. eSEXO = "M" .OR. eSEXO = SPACE(1)
               eTIPO_SAC = "01"
            ELSE
               eTIPO_SAC = "02" 
            ENDIF
            * CGC ou CPF Sacado
            eTAM_CGC_CPF = LEN(eCPF_CGC)
            eAUX_CGC     = SPACE(14)
            eAUX = 1
            DO WHILE eAUX <= eTAM_CGC_CPF
               IF SUBSTR(eCPF_CGC,eAUX,1) = "." .OR. SUBSTR(eCPF_CGC,eAUX,1) = "-" .OR. SUBSTR(eCPF_CGC,eAUX,1) = "/" .OR. SUBSTR(eCPF_CGC,eAUX,1) = " "
                  eAUX = eAUX + 1
               ELSE
                  eAUX_CGC = ALLTRIM(eAUX_CGC) + SUBSTR(eCPF_CGC,eAUX,1)
                  eAUX = eAUX + 1
               ENDIF
            ENDDO
            eCPF_CGC = ALLTRIM(eAUX_CGC)            
            * CEP do Sacado
            eTAM_CEP = LEN(ALLTRIM(eCEP))
            IF eTAM_CEP <> 0
               eCEP = REPLICATE("0",8-eTAM_CEP)+ALLTRIM(eCEP)
            ELSE 
               eCEP = REPLICATE("0",8)
            ENDIF
            * Numero Sequencial do Registro
            eTAM_SEQ = LENNUM(eNUM_SEQ)
            eREG_SEQ = REPLICATE("0",6-eTAM_SEQ)+STR(eNUM_SEQ,eTAM_SEQ,0)
            SELE 8
            DO REGISTRO
            SKIP
         ENDDO
         SELE 4
         SKIP
      ENDDO
      * Registro Header Final
      * Numero Sequencial do Registro

      HEADERFIM()

      SET PRINT OFF
      SELE 7
      USE BANCOS INDEX BANCOS
      GOTO TOP
      SEEK VAL(eNUM_BANCO)
      IF F_BLOQR()
         REPLACE ARQUIVO   WITH ARQUIVO + 1
         REPLACE SEQUENCIA WITH eSEQUENCIA
         UNLOCK
      ENDIF
   ENDIF
ENDIF
SET DEVICE TO SCREEN
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
FUNCTION HEADERINI()
      DO CASE
         CASE eNUM_BANCO = "001"
            ?? "01REMESSA01COBRANCA       "+SUBSTR(eCOD_AGENCI,1,5)+SUBSTR(eCOD_CC,1,9)+"000000"+eEMPRESA+;
            eNUM_BANCO+SUBSTR(eNOME_BANCO,1,15)+SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,2)+"00"+eNUM_ARQ+SPACE(22)+ALLTRIM(eCAIXA_POS)+SPACE(258)+"000001"
         CASE eNUM_BANCO = "008"
            ?? "01REMESSA01COBRANCA       "+SUBSTR(eCOD_AGENCI,1,5)+"000000"+SUBSTR(eCOD_CC,1,9)+"  "+eEMPRESA+;
            eNUM_BANCO+SUBSTR(eNOME_BANCO,1,15)+SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,2)+"01600BPI"+"0"+eNUM_ARQ+SPACE(280)+"000001"
         CASE eNUM_BANCO = "033"
            ?? "01REMESSA01COBRANCA       "+SUBSTR(eCOD_AGENCI,2,3)+SUBSTR(eCOD_CC,1,8)+SPACE(9)+eEMPRESA+;
            eNUM_BANCO+SUBSTR(eNOME_BANCO,1,7)+SPACE(8)+SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,2)+"01600BPI"+SPACE(286)+"000001"
         CASE eNUM_BANCO = "104"
            IF eASSUNTO = "SIGCB"  
		    * SIGCB
		    ?? "01REM.TST01COBRANCA       "+eCOD_AGENCI+eCOMPLEM+SPACE(10)+SUBSTR(eEMPRESA,1,30)+;
		    eNUM_BANCO+"C ECON FEDERAL "+SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,2)+SPACE(289)+eNUM_ARQ+"000001"
            ELSE
		    * SICOB
		    ?? "01REMESSA01COBRANCA       "+eCOD_AGENCI+eCOD_CC+SPACE(4)+SUBSTR(eEMPRESA,1,30)+;
		    eNUM_BANCO+eNOME_BANCO+SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,2)+SPACE(289)+eNUM_ARQ+"000001"            
            ENDIF
         CASE eNUM_BANCO = "151"
            * COBRANCA DIRETA
            ?? "15100000         2"+sisCGC_CPF+SPACE(20)+"00"+eCOD_AGENCI+"0000"+eCOD_CC+SPACE(1)+SUBSTR(eEMPRESA,1,30)+;
            "NOSSA CAIXA-NOSSO BANCO       "+SPACE(10)+"1"+SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,2)+"1200000"+eNUM_ARQ+"020"+"00000"+SPACE(69)
            * Numero Sequencial do Registro
            eNUM_SEQ = eNUM_SEQ + 1
            eTAM_SEQ = LENNUM(eNUM_SEQ)
            eREG_SEQ = REPLICATE("0",6-eTAM_SEQ)+STR(eNUM_SEQ,eTAM_SEQ,0)
            ?  "15100011R0100020 20"+sisCGC_CPF+SPACE(20)+"00"+eCOD_AGENCI+"0000"+eCOD_CC+SPACE(1)+SUBSTR(eEMPRESA,1,30)+;
            "000000"+SPACE(40)+SPACE(34)+"000"+eNUM_ARQ+SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,4)+"00000000"+SPACE(33)
         CASE eNUM_BANCO = "215"
            ?? "01REMESSA01COBRANCA"+SPACE(7)+eCOD_AGENCI+"02"+SUBSTR(eCOD_CC,1,8)+SPACE(6)+eEMPRESA+;
            eNUM_BANCO+eNOME_BANCO+SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,4)+"00000BPI"+"00100"+SPACE(281)+"000001"
         CASE eNUM_BANCO = "230"
            ?? "01REMESSA01COBRANCA"+SPACE(7)+SUBSTR(eCOD_AGENCI,2,3)+SUBSTR(eCOD_CC,1,10)+SPACE(7)+eEMPRESA+;
            eNUM_BANCO+eNOME_BANCO+SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,2)+"01600BPI"+SPACE(286)+"000001"
         CASE eNUM_BANCO = "237"
            ?? "01REMESSA01COBRANCA"+SPACE(7)+REPLICATE("0",13)+eCOMPLEM+eEMPRESA+;
            eNUM_BANCO+eNOME_BANCO+SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,2)+SPACE(8)+"MX00"+ENUM_ARQ+SPACE(11)+"232A"+SPACE(262)+"000001"
         CASE eNUM_BANCO = "275"
            ?? "01REMESSA01COBRANCA       "+"0"+SUBSTR(eCOD_AGENCI,1,4)+"0"+SUBSTR(eCOD_CC,1,7)+SPACE(7)+eEMPRESA+;
            eNUM_BANCO+SUBSTR(eNOME_BANCO,1,15)+SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,2)+"01600BPI"+SPACE(286)+"000001"
         CASE eNUM_BANCO = "291"
            eNUM_ARQ    = "0"+eNUM_ARQ
            ?? "01REMESSA01COBRANCA       "+SPACE(10)+SUBSTR(eCOD_AGENCI,2,3)+SUBSTR(eCOD_CC,1,7)+eEMPRESA+;
            eNUM_BANCO+eNOME_BANCO+SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,2)+"01600BPI"+SPACE(280)+eNUM_ARQ+"000001"
         CASE eNUM_BANCO = "341"
            ?? "01REMESSA01COBRANCA"+SPACE(7)+eCOD_AGENCI+eCOD_CC+SPACE(8)+eEMPRESA+;
            eNUM_BANCO+eNOME_BANCO+SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,2)+SPACE(294)+"000001"
         CASE eNUM_BANCO = "356"
            ?? "01REMESSA01COBRANCA       "+"0"+SUBSTR(eCOD_AGENCI,1,4)+"0"+SUBSTR(eCOD_CC,1,7)+SPACE(7)+eEMPRESA+;
            eNUM_BANCO+SUBSTR(eNOME_BANCO,1,15)+SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,2)+"01600BPI"+SPACE(286)+"000001"
         CASE eNUM_BANCO = "409"
            ?? "01REMESSA01COBRANCA       "+SUBSTR(eCOD_AGENCI,1,4)+SUBSTR(eCOD_CC,1,7)+SPACE(9)+eEMPRESA+;
            eNUM_BANCO+SUBSTR(eNOME_BANCO,1,15)+SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,2)+"01600BPI"+SPACE(283)+SUBSTR(eNUM_ARQ,3,3)+"000001"
         CASE eNUM_BANCO = "422"
            ?? "01REMESSA01COBRANCA       "+SUBSTR(eCOD_AGENCI,1,5)+SUBSTR(eCOD_CC,1,9)+SPACE(6)+eEMPRESA+;
            eNUM_BANCO+SUBSTR(eNOME_BANCO,1,11)+SPACE(4)+SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,2)+SPACE(291)+SUBSTR(eNUM_ARQ,3,3)+"000001"
         CASE eNUM_BANCO = "424"
            ?? "01REMESSA09COBRANCA"+SPACE(7)+eCOD_AGENCI+eCOD_CC+SPACE(9)+eEMPRESA+eNUM_BANCO+eNOME_BANCO+SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,2)+SPACE(294)+"000001"
         CASE eNUM_BANCO = "479"
            ?? "01REMESSA01COBRANCA       "+"009"+"01692724"+SPACE(9)+eEMPRESA+eNUM_BANCO+eNOME_BANCO+SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,2)+"01600BPI"+SPACE(91)+"00000000"+SPACE(187)+"000001"
         CASE eNUM_BANCO = "641"
            eNUM_ARQ    = SUBSTR(eNUM_ARQ,3,3)
            ?? "01REMESSA01COBRANCA"+SPACE(7)+REPLICATE("0",6)+sisCGC_CPF+eEMPRESA+eNUM_BANCO+eNOME_BANCO+SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,9,2)+SPACE(291)+eNUM_ARQ+"000001"
      ENDCASE
RETURN nil
****************************************************************************
FUNCTION HEADERFIM()
      eTAM_SEQ = LENNUM(eNUM_SEQ)
      eREG_SEQ = REPLICATE("0",6-eTAM_SEQ)+STR(eNUM_SEQ,eTAM_SEQ,0)
      DO CASE
         CASE eNUM_BANCO = "008"
            *
            eVLR_TIT = INT(ROUND((eVLR_TIT*100),0))
            eTAM_VLR = LENNUM(eVLR_TIT)
            eVLR_TIT = STR(eVLR_TIT,eTAM_VLR,0)
            eVLR_TIT = ALLTRIM(eVLR_TIT)
            eVLR_TIT = REPLICATE("0",13-eTAM_VLR)+ALLTRIM(eVLR_TIT)
            *
            eTAM_TIT = LENNUM(eTOT_TIT)
            eTOT_TIT = STR(eTOT_TIT,eTAM_TIT,0)
            eTOT_TIT = ALLTRIM(eTOT_TIT)
            eTOT_TIT = REPLICATE("0",8-eTAM_TIT)+ALLTRIM(eTOT_TIT)

            ?
            *
         CASE eNUM_BANCO = "104"
            *
            IF eASSUNTO = "SIGCB"   
            	    * SIGCB
		    ? "9"+SPACE(393)+eREG_SEQ
		    ?            
            ELSE
            	    * SICOB
		    ? "9"+SPACE(393)+eREG_SEQ
		    ?               
            ENDIF
            *
         CASE eNUM_BANCO = "151"
            *
            ? "15100015"+SPACE(9)+"0"+eREG_SEQ+SPACE(217)
            ? "15199999"+SPACE(9)+"000001"+eREG_SEQ+"000000"+SPACE(205)
            ?
            *
         CASE eNUM_BANCO = "275"
            *
            eVLR_TIT = INT(ROUND((eVLR_TIT*100),0))
            eTAM_VLR = LENNUM(eVLR_TIT)
            eVLR_TIT = STR(eVLR_TIT,eTAM_VLR,0)
            eVLR_TIT = ALLTRIM(eVLR_TIT)
            eVLR_TIT = REPLICATE("0",13-eTAM_VLR)+ALLTRIM(eVLR_TIT)
            *
            eTAM_TIT = LENNUM(eTOT_TIT)
            eTOT_TIT = STR(eTOT_TIT,eTAM_TIT,0)
            eTOT_TIT = ALLTRIM(eTOT_TIT)
            eTOT_TIT = REPLICATE("0",6-eTAM_TIT)+ALLTRIM(eTOT_TIT)
            ? "9"+eTOT_TIT+eVLR_TIT+SPACE(374)+eREG_SEQ
            ?
            *
         CASE eNUM_BANCO = "356"
            *
            eVLR_TIT = INT(ROUND((eVLR_TIT*100),0))
            eTAM_VLR = LENNUM(eVLR_TIT)
            eVLR_TIT = STR(eVLR_TIT,eTAM_VLR,0)
            eVLR_TIT = ALLTRIM(eVLR_TIT)
            eVLR_TIT = REPLICATE("0",13-eTAM_VLR)+ALLTRIM(eVLR_TIT)
            *
            eTAM_TIT = LENNUM(eTOT_TIT)
            eTOT_TIT = STR(eTOT_TIT,eTAM_TIT,0)
            eTOT_TIT = ALLTRIM(eTOT_TIT)
            eTOT_TIT = REPLICATE("0",6-eTAM_TIT)+ALLTRIM(eTOT_TIT)
            ? "9"+eTOT_TIT+eVLR_TIT+SPACE(374)+eREG_SEQ
            ?
            *

         CASE eNUM_BANCO = "422"
            *
            eVLR_TIT = INT(ROUND((eVLR_TIT*100),0))
            eTAM_VLR = LENNUM(eVLR_TIT)
            eVLR_TIT = STR(eVLR_TIT,eTAM_VLR,0)
            eVLR_TIT = ALLTRIM(eVLR_TIT)
            eVLR_TIT = REPLICATE("0",15-eTAM_VLR)+ALLTRIM(eVLR_TIT)
            *
            eTAM_TIT = LENNUM(eTOT_TIT)
            eTOT_TIT = STR(eTOT_TIT,eTAM_TIT,0)
            eTOT_TIT = ALLTRIM(eTOT_TIT)
            eTOT_TIT = REPLICATE("0",8-eTAM_TIT)+ALLTRIM(eTOT_TIT)
            ? "9"+SPACE(367)+eTOT_TIT+eVLR_TIT+SUBSTR(eNUM_ARQ,3,5)+eREG_SEQ
            ?
            *
         OTHERWISE
            *
            ? "9"+SPACE(393)+eREG_SEQ
            ?
            *
      ENDCASE
RETURN nil
*************************  fim  *********************************
