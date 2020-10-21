*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_NPRO.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE NOTAS PROMISSORIAS        
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_NPRO()
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
PLQUA("01")
eOPCAO = SPACE(1)
@ 23,00 SAY "Imprimir Todas ou s¢ as em Aberto (T/A) ? " GET eOPCAO PICT "@!" VALID eOPCAO $("TA")
READ
PLQUA("01")
eBASE = SPACE(1)
@ 23,00 SAY "Data Base na Venda ou no Vencimento (V/P) ? " GET eBASE PICT "@!" VALID eBASE $("VP")
READ
PLQUA("01")
PPERG ( "Deseja Razao Social e CNPJ diferentes dos Cadastrados (S/N) ? ")
IF sisRESP = "N"
   eEMPRESA = REPLICATE(" ",47-LEN(sisTIT1))+sisTIT1
   eCGC     = sisCGC_CPF
ELSE
   eEMPRESA = SPACE(47)
   eCGC     = SPACE(14)
   PLQUA("01")
   @ 23,00 SAY "Razao Social : " GET eEMPRESA
   READ
   PLQUA("01")
   @ 23,00 SAY "CNPJ : " GET eCGC
   READ
ENDIF
eCONJUGE = "N"
e2NOME   = SPACE(50)
e2CPF    = SPACE(20)
PLQUA("01")
PPERG ( "Deseja dados do Conjuge (S/N) ? ")
IF sisRESP = "S"
   eCONJUGE = "S"
ENDIF
PLQUA("01")
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   @ 23,02 SAY "Periodo : " GET eDATA_INI
   @ 23,25 SAY "Ate : "     GET eDATA_FIM
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
   SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE .AND. ATIVA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
   GOTO TOP
ELSE
   USE &eARQ INDEX &eIND2,&eIND3,&eIND1
   SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE .AND. DATA_PGTO = CTOD("") .AND. ATIVA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
   GOTO TOP
ENDIF

eARQUIVO = SPACE(60)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"PROMISSORIAS.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "PROMISSORIAS.TXT"+SPACE(10) 
ENDIF   

eSAIDA   = SPACE(10)
DO WHILE .T.
   PLQUA("01")
   PPERG ( "Deseja promiss¢rias em arquivo texto no Drive (X:) (S/N) ? ")
   IF sisRESP = "S"
      PLQUA("01")
	  @ 23,00 SAY "CONFIRME O DESTINO  : " GET eARQUIVO
      READ
      eARQUIVO = ALLTRIM(eARQUIVO)
      eSAIDA = "ARQUIVO"
      EXIT
   ELSE
      DO PIMPR
      eSAIDA = "IMPRESSORA"
      EXIT
   ENDIF
ENDDO   
SELE 8
GOTO TOP  
IF EOF()
   PLQUA("01")
   PMSGR( "Atená∆o !",3)
   PMENSAG("Nao H† Notas Promiss¢rias a Imprimir !")
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
   LIN = 01
   SELE 8
   GOTO TOP   
   DO WHILE .NOT. EOF()
      DO WHILE .NOT. EOF() .AND. LIN <= 46
         @ LIN,00  SAY "NOTA PROMISSORIA EM REAIS"
         IF eSAIDA <> "ARQUIVO"         
            @ LIN,00  SAY "NOTA PROMISSORIA EM REAIS"
         END
         @ LIN,40  SAY "VINCULADA AO CONTRATO DE COMPRA E VENDA"
         LIN = LIN + 1
         IF (eDESC_PRO <> "")
	         @ LIN,00  SAY ALLTRIM(eDESC_PRO)
	 ENDIF
         @ LIN,40  SAY "DA UNIDADE No. : "+ALLTRIM(eQUADRA)+"/"+ALLTRIM(eNUM_IMOVEL)
         LIN = LIN + 1
         SELE 6
         SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)      
         ePARCELAS   = STR(PARCELAS,5)
         eINDICE     = INDICE
         eDATA_VENDA = DTOC(DATA_VENDA)
         SELE 8
         @ LIN,00  SAY "PROMISSORIA No. : "
         @ LIN,20  SAY ALLTRIM(NUM_PARC)+"/"+ALLTRIM(ePARCELAS)+"   - "+ALLTRIM(TIPO_PARC)

         
         LIN = LIN + 1
         @ LIN,00  SAY "VENCIMENTO      : "
         @ LIN,20  SAY DATA_VENC
         SELE 6
         @ LIN,40  SAY "DATA DO CONTRATO : "+DTOC(DATA_VENDA)
         SELE 8
         LIN = LIN + 1
         @ LIN,00  SAY "VALOR ("+sisMOEDA+")      : "
         @ LIN,20  SAY VLR_PARC   PICT "@R 999,999.99"
         LIN = LIN + 2
         eDATA_VENC = DATA_VENC
         eVLR_PARC = VLR_PARC
         pNUM_EXT1 = SPACE(76)
         pNUM_EXT2 = SPACE(76)
         pNUM_EXT3 = SPACE(76)
         eDATA = DTOC(DATA_VENC)
         DO PDATAEXT WITH eDATA
         sisEXT    = REPLICATE(" ",35-LEN(sisEXT))+sisEXT
         @ LIN,10  SAY "AOS "+sisEXT+",  PAGAREI(EMOS) POR  ESTA"
         LIN = LIN + 1
         @ LIN,10  SAY "VIA DE NOTA PROMISSORIA A "+ALLTRIM(eEMPRESA)
         IF eSAIDA <> "ARQUIVO"            
            @ LIN,10  SAY "                          "+ALLTRIM(eEMPRESA)
         END         
         LIN = LIN + 1
         @ LIN,10  SAY "SOB O CNPJ No.  "+eCGC+", OU A SUA  ORDEM EM MOEDA CORRENTE" 
         LIN = LIN + 1
         @ LIN,10  SAY "NACIONAL,VALOR QUE NA DATA DE SUA EFETIVA LIQUIDACAO CORRESPONDER"
         LIN = LIN + 1
         @ LIN,10  SAY "A QUANTIA ABAIXO, CORRIGIDA DE ACORDO COM A VARIACAO DO "+ALLTRIM(eINDICE)
         LIN = LIN + 1
         IF eBASE = "V"
            @ LIN,10  SAY "COM DATA BASE EM "+eDATA_VENDA
         ELSE
            @ LIN,10  SAY "COM DATA BASE EM "+eDATA
         ENDIF
         LIN = LIN + 1
         @ LIN,10  SAY sisMOEDA
         @ LIN,13  SAY VLR_PARC   PICT "@R 999,999.99"
         @ LIN,23  SAY "."
         LIN = LIN + 1
         DO PNUMEXT WITH eVLR_PARC,58
         @ LIN,10  SAY "[**"+pNUM_EXT1+"***]"
         LIN = LIN + 1
         @ LIN,10  SAY "[**"+pNUM_EXT2+"***]"
         LIN = LIN + 1
         @ LIN,10  SAY "[**"+pNUM_EXT3+"***]"
         LIN = LIN + 1
         @ LIN,10  SAY "NA PRACA DO MUNICIPIO DE "+ALLTRIM(sisCIDADE)+" - "+ALLTRIM(sisESTADO)+"."
         LIN = LIN + 2
         @ LIN,00  SAY "EMITENTE : "
         IF eSAIDA <> "ARQUIVO"
         	@ LIN,00  SAY "EMITENTE : "
         END
         eDATA = DTOC(DATE())
         DO PDATAEXT WITH eDATA
         @ LIN,30  SAY sisCIDADE+", "+sisEXT+"."
         IF eSAIDA <> "ARQUIVO"
         	@ LIN,30  SAY sisCIDADE+", "+sisEXT+"."
         END
         LIN = LIN + 3
         SELE 5
         SEEK eNUM_CLIE
         eOBSERVA_4 = OBSERVA_4
         eOBSERVA_5 = OBSERVA_5
         @ LIN,00  SAY ALLTRIM(CLIENTE)
         IF eCONJUGE ="S"
            @ LIN,40  SAY ALLTRIM(OBSERVA_1)
         ENDIF
         LIN = LIN + 1
         @ LIN,00  SAY "CPF/CGC : "+CPF_CGC
         IF eCONJUGE ="S"
            @ LIN,40  SAY "CPF/CGC : "+ALLTRIM(OBSERVA_2)
         ELSE
            @ LIN,40 SAY REPLICATE("_",35)
            IF eSAIDA <> "ARQUIVO"            
               @ LIN,40 SAY REPLICATE("_",35)
            END
         ENDIF
         LIN = LIN + 1
         IF OPCAO = "R"
            @ LIN,00  SAY ALLTRIM(END_R)+" - "+ALLTRIM(BAIRRO_R)
            LIN = LIN + 1
            @ LIN,00  SAY CEP_R PICT "@R 99999-999"
            LIN = LIN + 1
            @ LIN,00  SAY ALLTRIM(CIDADE_R)+" - "+ESTADO_R
            @ LIN,40 SAY REPLICATE("_",35)
            IF eSAIDA <> "ARQUIVO"            
               @ LIN,40 SAY REPLICATE("_",35)
            END
         ELSE
            @ LIN,00  SAY ALLTRIM(END_C)+" - "+ALLTRIM(BAIRRO_C)
            LIN = LIN + 1
            @ LIN,00  SAY CEP_C PICT "@R 99999-999"
            LIN = LIN + 1
            @ LIN,00  SAY ALLTRIM(CIDADE_C)+" - "+ESTADO_C
            @ LIN,40 SAY REPLICATE("_",35)
            IF eSAIDA <> "ARQUIVO"            
               @ LIN,40 SAY REPLICATE("_",35)
            END
         ENDIF
         LIN = LIN + 2
         @ LIN,00 SAY REPLICATE(" _",35)
         LIN = LIN + 2
         SELE 8       
         SKIP
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
      LIN = 01
      @ 00,00 SAY " "
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
RESTORE SCREEN FROM TELA03
RETURN nil
************************  fim  *********************************















