*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_RECI.PRG
* Data        : 01/01/99
* Objetivo    : IMPRESSAO DA RELATORIO RECIBO DE PAGAMENTO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_RECI()
SAVE SCREEN TO TELA03
PTIT("EMISS«O DE RECIBO DE PAGAMENTO")
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
      DO WHILE .T. 
         * Consulta de Loteamentos
         SELE 3
         SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
         GOTO TOP
         eQUADRA   = 0
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
            PMENSAG("N∆o H† "+sisGRUPO1+" Vendidos !")
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
               PMENSAG ( sisUNID1+" n∆o Vendido !")
               PPARA()
            ELSE
               ePARTE = 0
               F_RECI(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,ePARTE,ePROPR_1)
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
                  PMENSAG ( sisGRUPO1+" n∆o Vendido !")
                  PPARA()
               ELSE
                  ePARTE = 1
                  F_RECI(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,ePARTE,ePROPR_1)
               ENDIF  
            ELSE
               IF ePROPR_2 = 1
                  PLQUA("01")
                  PMSGR( "Atená∆o !",3)
                  PMENSAG ( sisGRUPO1+" n∆o Vendido !")
                  PPARA()
               ELSE
                  ePARTE = 2
                  F_RECI(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,ePARTE,ePROPR_2)
               ENDIF  
            ENDIF
         ENDIF
         PLQUA("01")
         PPERG ( "Deseja Continuar no Mesmo Produto (S/N) ? ")
         IF sisRESP = "S"
            @ 06,00 CLEAR TO 21,79
            LOOP
         ELSE
            EXIT
         ENDIF
      ENDDO
   CASE eTIPO_PRO = 2
      DO WHILE .T.
         * Consulta de Apartamentos
         SELE 4
         SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
         GOTO TOP
         eBLOCO    = 0
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
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1))
         GOTO TOP
         IF EOF()
            PLQUA("01")
            PMSGR( "Atená∆o !",3)
            PMENSAG("N∆o H† "+sisUNID1+" Vendidos !")
            PPARA()
            CLOSE DATABASES
            RESTORE SCREEN FROM TELA03
            RETURN nil
         ENDIF 
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
         CAB[2]="Bloco"
         CAB[3]="Apto."
         CAB[4]="Status"
         CAB[5]="Proprietario"
         DBEDIT(7,14,7,64,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
         eNUM_IMOVEL = NUM_IMOVEL
         eSTATUS     = STATUS
         ePROPR_1    = PROPRIET_1
         ePARTE      = 0
         IF ePROPR_1 = 1
            PLQUA("01")
            PMSGR( "Atená∆o !",3)
            PMENSAG ( sisUNID2+" nao Vendido !")
            PPARA()
         ELSE
            F_RECI(eCLAS_PRO,eBLOCO,eNUM_IMOVEL,0,ePROPR_1) 
         ENDIF
         PLQUA("01")
         PPERG ( "Deseja Continuar no Mesmo Produto (S/N) ? ")
         IF sisRESP = "S"
            @ 06,00 CLEAR TO 21,79
            LOOP
         ELSE
            EXIT
         ENDIF
      ENDDO
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
SET PRINT TO
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION F_RECI
PARAMETERS pCLASSE,pQUADRA,pUNIDADE,pPARTE,pNUM_CLIE
eMSG = "RELACAO DAS PARCELAS DA UNIDADE No. "+ALLTRIM(pUNIDADE)+" - "+IIF(pPARTE=0,"Proprietario Unico",IIF(pPARTE=1,"1o. PROPRIETARIO","2o. PROPRIETARIO"))
PCENTRA( eMSG,9
SELE 5
USE CONTATO INDEX NUM_CLIE,CONTATO
GOTO TOP
SEEK pNUM_CLIE
@ 08,02 SAY "Cliente : "+CLIENTE
eCLIENTE = CLIENTE
eSEXO    = SEXO
SELE 8
eIND_CLAS = STR(pCLASSE,5)
eIND_QUAD = pQUADRA
eZERO = "000"
eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
USE &eARQ INDEX &eIND2,&eIND1,&eIND3
GOTO TOP
GOTO TOP
SET FILTER TO CLASSE = STR(pCLASSE,5) .AND. QUADRA = pQUADRA .AND. NUM_IMOVEL = pUNIDADE .AND. PARTE = pPARTE
GOTO TOP
DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
AFIELDS(CAMPO,.T.,LENS)
CAMPO[1]="NUM_PARC"
CAMPO[2]="TIPO_PARC"
CAMPO[3]="DATA_VENC"
CAMPO[4]="VLR_PARC"
CAMPO[5]="DATA_PGTO"
CAMPO[6]="VLR_PGTO"
LENS[1]=5
LENS[2]=3
LENS[3]=8
LENS[4]=19
LENS[5]=8
LENS[6]=19
PIC[1]="@R99999"
PIC[2]="XXX"
PIC[3]="99/99/99"
PIC[4]="@R 99,999,999,999.99"
PIC[5]="99/99/99"
PIC[6]="@R 99,999,999,999.99"
CAB[1]="Parcela"
CAB[2]="Tipo(N/I)"
CAB[3]="Venc."
CAB[4]="Parcela"
CAB[5]="Pago"
CAB[6]="Vlr. Pagamento"
@ 10,00 TO 21,79 DOUBLE
DBEDIT(11,01,21,78,CAMPO,"FUNC_RECI",PIC,CAB,CHR(196),CHR(179),CHR(205))
RETURN nil 0
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION FUNC_RECI
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
         SAVE SCREEN TO TELA999
         PLQUA("01")
         PPERG ( "Confirma a Emiss∆o do Recibo (S/N) ? ")
         IF sisRESP = "S"
            SELE 9
            USE STATUS
            GOTO TOP
            eNUM_RECI = 0
            eNUM_RECI = NUM_RECI
            eCIDADE   = CIDADE
            eRECIBO = 3
            eVEZES  = 0
            
		eARQUIVO = SPACE(60)
		IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
			eARQUIVO = sisDRIVE_REL+":"
		ELSE
			eARQUIVO = "C:"
		ENDIF	
		
		IF sisPASTA_REL <> SPACE(30)
			eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"VENDAS.TXT"+SPACE(10)  
		ELSE
			eARQUIVO = ALLTRIM(eARQUIVO) + "VENDAS.TXT"+SPACE(10) 
		ENDIF       
		eSAIDA   = SPACE(10)
		DO WHILE .T.
		   PLQUA("01")
		   PPERG ( "Deseja Relat¢rio em arquivo texto no Drive (X:) (S/N) ? ")
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
	   
	    LIN  = 0
            ePAG = 1
            DO WHILE eVEZES < eRECIBO
               @ LIN,00  SAY sisLETRAC+"Empresa : "+sisTIT1
               @ LIN,120 SAY "Data : "+dtoc(DATE())
               LIN = LIN + 1
               @ LIN,00  SAY "Sistema : "+sisTIT2
               @ LIN,121 SAY "Relat: SCR004"
               LIN = LIN + 1
               @ LIN,30  SAY sisLETRAE+"RECIBO PAGAMENTO DE PARCELAS No. "+ALLTRIM(STR(INT(eNUM_RECI)))+sisLETRAC
               LIN = LIN + 1
               @ LIN,00  SAY REPLICATE("_",135)
               LIN = LIN + 1
               @ LIN,00  SAY "|"
               @ LIN,134 SAY "|"
               LIN = LIN + 1
               @ LIN,00  SAY "|"
               SELE 2
               SEEK pCLASSE
               @ LIN,05  SAY "Empreendimento : "+ALLTRIM(STR(pCLASSE,5))+" - "+ALLTRIM(DESCRICAO)
               IF eTIPO_PRO = 1
                  @ LIN,60  SAY "Quadra : "+ALLTRIM(pQUADRA)
                  @ LIN,80  SAY "Lote   : "+ALLTRIM(pUNIDADE)
                  @ LIN,95  SAY "Parte  : "+STR(pPARTE,1)
               ELSE
                  SELE 4
                  @ LIN,60  SAY "Bloco     : "+ALLTRIM(pQUADRA)+" - "+DESCRICAO
                  @ LIN,100 SAY "Apto./Sala: "+ALLTRIM(pUNIDADE)
               ENDIF
               @ LIN,134 SAY "|"
               LIN = LIN + 1
               @ LIN,00  SAY "|"
               @ LIN,134 SAY "|"
               LIN = LIN + 1
               @ LIN,00  SAY "|"
               IF eSEXO = "M"
                  @ LIN,20  SAY "Recebi do Sr. "+eCLIENTE+" a importancia  de "
               ELSE
                  IF eSEXO = "F"
                     @ LIN,20  SAY "Recebi da Sra. "+eCLIENTE+" a  importancia  de "
                  ELSE
                     @ LIN,20  SAY "Recebi de "+eCLIENTE+" a   importancia  de "
                  ENDIF
               ENDIF
               @ LIN,134 SAY "|"
               LIN = LIN + 1
               @ LIN,00  SAY "|"
               SELE 8
               @ LIN,20 SAY sisMOEDA
               @ LIN,34 SAY VLR_PGTO PICT "@R 999,999,999.99"
               @ LIN,49 SAY ", Referente a pagamento da parcela No. "+ALLTRIM(NUM_PARC)+" com vencimento"
               @ LIN,134 SAY "|"
               LIN = LIN + 1
               @ LIN,00  SAY "|"
               eDT_VENC = DATA_VENC
               SELE 2
               @ LIN,20 SAY "para "+DTOC(eDT_VENC)+"."
               @ LIN,134 SAY "|"
               LIN = LIN + 1
               @ LIN,00  SAY "|"
               @ LIN,134 SAY "|"
               LIN = LIN + 1
               SELE 8
               @ LIN,00 SAY "|"
               @ LIN,20 SAY "Valor da Prestacao : "
               @ LIN,40 SAY VLR_PARC   PICT "@R 999,999,999,999.99"
               @ LIN,134 SAY "|"
               LIN = LIN + 1
               @ LIN,00  SAY "|"
               @ LIN,20 SAY "Multa/Desconto     : "
               eDIFERENCA = VLR_PGTO - VLR_PARC
               @ LIN,40 SAY eDIFERENCA PICT "@R 999,999,999,999.99"
               @ LIN,134 SAY "|"
               LIN = LIN + 1
               @ LIN,00  SAY "|"
               @ LIN,20 SAY "Valor Pagamento    : "
               @ LIN,40 SAY VLR_PGTO   PICT "@R 999,999,999,999.99"
               @ LIN,134 SAY "|"
               LIN = LIN + 1
               @ LIN,00  SAY "|"
               @ LIN,70  SAY "----------------------------------------------"
               @ LIN,134 SAY "|"
               LIN = LIN + 1
               @ LIN,00  SAY "|"
               @ LIN,70  SAY "      "+sisTIT1
               @ LIN,70  SAY "      "+sisTIT1
               @ LIN,134 SAY "|"
               LIN = LIN + 1
               @ LIN,00  SAY "|"
               @ LIN,134 SAY "|"
               LIN = LIN + 1
               @ LIN,00  SAY "|"
               @ LIN,20  SAY "Banco : ________________________ No. Cheque : ___________________"
               @ LIN,134 SAY "|"
               LIN = LIN + 1
               @ LIN,00  SAY REPLICATE("_",135)
               LIN = LIN + 2
               eVEZES = eVEZES + 1
            ENDDO
            eNUM_RECI = eNUM_RECI + 1
            SELE 9
            
		IF eSAIDA = "ARQUIVO"
		   SET PRINT TO 
		   SET PRINT OFF
		   SET DEVICE TO SCREEN
		ELSE
		   @ 00,00 SAY " "		
		   SET PRINT TO
		   SET DEVICE TO SCREEN			   
		ENDIF 
		
            IF F_BLOQR()
               REPLACE NUM_RECI WITH eNUM_RECI
            ENDIF
         ENDIF
         RESTORE SCREEN FROM TELA999
      CASE KEY == 27
         RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
