*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_NPR2.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE NOTAS PROMISSORIAS        
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_NPR2()
SAVE SCREEN TO TELA03
PTIT( "EMISS«O DE NOTAS PROMISS‡RIAS")
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
eQUADRA  = SPACE(1)
DO CASE
   CASE eTIPO_PRO = 1
      * Consulta de Loteamentos
      SELE 3
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA     = SPACE(5)
      eNUM_IMOVEL = SPACE(5)
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
         PMENSAG("N∆o H† "+sisUNID1+" Disponá√≠veis "!")
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
      DBEDIT(7,15,7,69,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
      eNUM_IMOVEL = NUM_IMOVEL
      eSTATUS     = STATUS
      ePROPR_1    = PROPRIET_1
      ePROPR_2    = PROPRIET_2
      IF eSTATUS = 0
	 IF ePROPR_1 = 1
	    PLQUA("01")
	    PMSGR( "Atená∆o !",3)
            PMENSAG ( sisUNID1+" N∆o Vendido !")
	    PPARA()
	 ELSE
	    ePARTE = 0
	 ENDIF
      ELSE
	 eOPCAO = 0
	 PLQUA("01")
	 @ 23,00 SAY "Informe se (1)o. ou (2)o. Meio Lote ? " GET eOPCAO PICT "99999" RANGE 1,2
	 READ
	 IF eOPCAO = 1
	    IF ePROPR_1 = 1
	       PLQUA("01")
		  PMSGR( "Atená∆o !",3)
                  PMENSAG ( sisUNID1+" N∆o Vendido !")
		  PPARA()
	       ELSE
		  ePARTE = 1
	       ENDIF
	    ELSE
	       IF ePROPR_2 = 1
		  PLQUA("01")
		  PMSGR( "Atená∆o !",3)
                  PMENSAG ( sisUNID1+" N∆o Vendido !")
		  PPARA()
	       ELSE
		  ePARTE = 2
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
      eQUADRA      = NUM_BLOCO
      eQUANTIDADE = QUANTIDADE
      @ 07,02 SAY sisUNID2+" : "
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
      GOTO TOP
      IF EOF()
	 PLQUA("01")
	 PMSGR( "Atená∆o !",3)
         PMENSAG("N∆o H† "+sisUNID2+" Disponá√≠veis "!")
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
      DBEDIT(7,15,7,69,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
      eNUM_IMOVEL = NUM_IMOVEL
      eSTATUS     = STATUS
      ePROPR_1    = PROPRIET_1
      ePARTE      = 0
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
SELE 1
IF ePARTE = 0 .OR. ePARTE = 1
   eNUM_CLIE = PROPRIET_1
ELSE
   eNUM_CLIE = PROPRIET_2
ENDIF
SELE 6
SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE
GOTO TOP
eDATA_VENDA = DATA_VENDA
PLQUA("01")
eOPCAO = SPACE(1)
@ 23,00 SAY "Imprimir Todas ou em Aberto (T/A) ? " GET eOPCAO PICT "@!") VALID eOPCAO $("TA")
READ
eTAMANHO = SPACE(1)
@ 23,00 SAY "Nota promissoria formato (P)equeno ou (G)rande ? " GET eTAMANHO PICT "@!") VALID eTAMANHO $("PG")
READ
SELE 8
eIND_CLAS = STR(eCLAS_PRO,5)
eIND_QUAD = eQUADRA
eZERO = "000"
eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
IF eOPCAO = "T"
   USE &eARQ INDEX &eIND2,&eIND3,&eIND1
   SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE
   GOTO TOP
ELSE
   USE &eARQ INDEX &eIND2,&eIND3,&eIND1
   SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE .AND. DATA_PGTO = CTOD("")
   GOTO TOP
ENDIF
IF EOF()
   PLQUA("01")
   PMSGR( "Atená∆o !",3)
   PMENSAG("Nao H† Notas Promissárias a Imprimir !")
   PPARA()
ELSE
   IF eTAMANHO = "P"
      PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
      PPARA()
      DO PIMPR
      SET DEVICE TO PRINT
      eLIN = PROW()
      DO WHILE .NOT. EOF()
         eLIN = eLIN + 1
         @ eLIN,00  SAY sisLETRAC+" "
         @ eLIN,35  SAY ALLTRIM(NUM_PARC)+"/"+ALLTRIM(TIPO_PARC)
         eDATA = DTOC(DATA_VENC)
         DO PDATAEXT WITH eDATA
         sisEXT    = REPLICATE(" ",35-LEN(sisEXT))+sisEXT
         @ eLIN,72  SAY sisEXT
*        @ eLIN,20  SAY DATA_VENC
         eLIN = eLIN + 2
         @ eLIN,120 SAY VLR_PARC   PICT "@R 999,999.99"
         eLIN = eLIN + 4
         @ eLIN,30 SAY sisTIT1
         eLIN = eLIN + 2
         @ eLIN,110 SAY sisCGC_CPF
         eLIN = eLIN + 2
         eDATA_VENC = DATA_VENC
         eVLR_PARC = VLR_PARC
         pNUM_EXT1 = SPACE(76)
         pNUM_EXT2 = SPACE(76)
         pNUM_EXT3 = SPACE(76)
         eDATA = DTOC(DATA_VENC)
         DO PNUMEXT WITH eVLR_PARC,58
         @ eLIN,40  SAY "(**"+pNUM_EXT1+"***)"
         eLIN = eLIN + 1
         @ eLIN,40  SAY "(**"+pNUM_EXT2+"***)"
         eLIN = eLIN + 1
         @ eLIN,40  SAY "(**"+pNUM_EXT3+"***)"
         eLIN = eLIN + 2
         @ eLIN,40 SAY sisCIDADE
         eDATA = DTOC(eDATA_VENDA)
         DO PDATAEXT WITH eDATA
         @ eLIN,95 SAY sisCIDADE+", "+ALLTRIM(sisEXT)
         eLIN = eLIN + 1
         SELE 5
         SEEK eNUM_CLIE
         @ eLIN,40  SAY ALLTRIM(CLIENTE)
         eLIN = eLIN + 1
         @ eLIN,30  SAY "CPF : "+CPF_CGC
         eLIN = eLIN + 1
         IF OPCAO = "R"
            @ eLIN,50  SAY ALLTRIM(END_R)+" - "+ALLTRIM(BAIRRO_R)
            eLIN = eLIN + 1
            @ eLIN,50  SAY CEP_R PICT "@R 99999-999"
            eLIN = eLIN + 1
            @ eLIN,50  SAY ALLTRIM(CIDADE_R)+" - "+ESTADO_R
            eLIN = eLIN + 1
         ELSE
            @ eLIN,50  SAY ALLTRIM(END_C)+" - "+ALLTRIM(BAIRRO_C)
            eLIN = eLIN + 1
            @ eLIN,50  SAY CEP_C PICT "@R 99999-999"
            eLIN = eLIN + 1
            @ eLIN,50  SAY ALLTRIM(CIDADE_C)+" - "+ESTADO_C
            eLIN = eLIN + 1
         ENDIF
         eLIN = eLIN + 1
         @ eLIN,00 SAY " "+sisLETRAN
         eLIN = eLIN + 4
         SELE 8
         SKIP
         DO PTECLA
         IF sisINTERR
            SET DEVICE TO SCREEN
            CLOSE ALL
            RETURN nil
         ENDIF
      ENDDO
   ELSE
      PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
      PPARA()
      DO PIMPR
      SET DEVICE TO PRINT
      eLIN = PROW()
      DO WHILE .NOT. EOF()
         eLIN = eLIN + 1
         eDATA = DTOC(eDATA_VENDA)
         DO PDATAEXT WITH eDATA
         @ eLIN,40 SAY sisCIDADE+", "+ALLTRIM(sisEXT)
*        @ eLIN,20  SAY DATA_VENC
         eLIN = eLIN + 3
         @ eLIN,20  SAY ALLTRIM(NUM_PARC)+"/"+ALLTRIM(TIPO_PARC)
         @ eLIN,80  SAY VLR_PARC   PICT "@R 999,999.99"
         eLIN = eLIN + 2
         eDATA = DTOC(DATA_VENC)
         DO PDATAEXT WITH eDATA
         @ eLIN,20 SAY ALLTRIM(sisEXT)
         eLIN = eLIN + 4
         @ eLIN,30 SAY sisTIT1
         @ eLIN,60 SAY "CGC No. : "+sisCGC_CPF
         eLIN = eLIN + 5
         eDATA_VENC = DATA_VENC
         eVLR_PARC = VLR_PARC
         pNUM_EXT1 = SPACE(76)
         pNUM_EXT2 = SPACE(76)
         pNUM_EXT3 = SPACE(76)
         eDATA = DTOC(DATA_VENC)
         DO PNUMEXT WITH eVLR_PARC,58
         @ eLIN,20  SAY "(**"+pNUM_EXT1+"***)"
         eLIN = eLIN + 1
         @ eLIN,20  SAY "(**"+pNUM_EXT2+"***)"
         eLIN = eLIN + 1
         @ eLIN,20  SAY "(**"+pNUM_EXT3+"***)"
         eLIN = eLIN + 3
         SELE 5
         SEEK eNUM_CLIE
         eOBSERVA_4 = OBSERVA_4
         eOBSERVA_5 = OBSERVA_5
         @ eLIN,20  SAY ALLTRIM(CLIENTE)
         @ eLIN,70  SAY "CPF : "+CPF_CGC
         eLIN = eLIN + 1
         IF OPCAO = "R"
            @ eLIN,20  SAY ALLTRIM(END_R)+" - "+ALLTRIM(BAIRRO_R)
            eLIN = eLIN + 1
            @ eLIN,20  SAY CEP_R PICT "@R 99999-999"
            @ eLIN,30  SAY " - "+ALLTRIM(CIDADE_R)+" - "+ESTADO_R
            eLIN = eLIN + 2
         ELSE
            @ eLIN,20  SAY ALLTRIM(END_C)+" - "+ALLTRIM(BAIRRO_C)
            eLIN = eLIN + 1
            @ eLIN,20  SAY CEP_C PICT "@R 99999-999"
            @ eLIN,30  SAY "- "+ALLTRIM(CIDADE_C)+" - "+ESTADO_C
            eLIN = eLIN + 2
         ENDIF
         @ eLIN,30 SAY sisCIDADE
         eLIN = eLIN + 1
         @ eLIN,15  SAY eOBSERVA_4
         eLIN = eLIN + 1
         @ eLIN,15  SAY eOBSERVA_5
         eLIN = eLIN + 2
         @ eLIN,00 SAY " "
         eLIN = eLIN + 5
         SELE 8
         SKIP
         DO PTECLA
         IF sisINTERR
            SET DEVICE TO SCREEN
            CLOSE ALL
            RETURN nil
         ENDIF
      ENDDO
   ENDIF
ENDIF
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
