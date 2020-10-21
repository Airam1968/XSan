*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_PAGA.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE RECEBIMENTOS NO PERIODO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_PAGA()
SAVE SCREEN TO TELA03
PTIT( "RELAT‡RIO DE RECEBIMENTOS")
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
USE CONTAS INDEX CONTAS
GOTO TOP
SELE 8
* Cadastro de Parcelas
SELE 2
eCLASSE     = SPACE(5)
eQUADRA     = SPACE(5)
eNUM_IMOVEL = SPACE(5)
ePARTE      = 0
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
         PLQUA("01")
         PPERG ( "Deseja escolher a Unidade (S/N) ? ")
         IF sisRESP = "S"
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
                  SELE 1
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. STATUS = 0 
                  GOTO TOP
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
                     SELE 1
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. STATUS = 1
                     GOTO TOP
                  ENDIF
               ELSE
                  IF ePROPR_2 = 1
                     PLQUA("01")
                     PMSGR( "Atená∆o !",3)
                     PMENSAG ( sisUNID1+" N∆o Vendido !")
                     PPARA()
                  ELSE
                     ePARTE = 2
                     SELE 1
                     SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. STATUS = 1
                     GOTO TOP
                  ENDIF
               ENDIF
            ENDIF
         ELSE
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA
            GOTO TOP
            eNUM_IMOVEL = SPACE(5)
            eSTATUS     = 0
            ePROPR_1    = 0
            ePROPR_2    = 0
            ePARTE      = 0
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
         PLQUA("01")
         PPERG ( "Deseja escolher a Unidade (S/N) ? ")
         IF sisRESP = "S"
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
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. STATUS = 0
            GOTO TOP
         ELSE
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA
            GOTO TOP
            eNUM_IMOVEL = SPACE(5)
            eSTATUS     = 0
            ePROPR_1    = 0
            ePARTE      = 0
         ENDIF
      ENDIF
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   @ 08,02 SAY "Periodo : " GET eDATA_INI
   @ 08,25 SAY "Ate : "     GET eDATA_FIM
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
PLQUA("01")
eORIGEM = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Origem dos Pagamentos (C)arteira,(B)anco ou (A)mbas ? " GET eORIGEM PICT "@!X" VALID eORIGEM $("CBA")
READ
PLQUA("01")
eTIPO = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Tipo de Parcelas (S)inal,(N)ormais,(I)ntermediarias ou (A)mbas ? " GET eTIPO PICT "@!X" VALID eTIPO $("SNIA")
READ
PLQUA("01")
eORDEM = SPACE(1)
@ 23,00 SAY "Emiss∆o de Relat¢rio por (O)rdem de Pagamento ou (I)movel (O/I) ? " GET eORDEM PICT "@!X" VALID eORDEM $("OI")
READ
PLQUA("01")
ePERIODO = SPACE(1)
@ 23,00 SAY "Pagamentos (A)trazado, (P)ostergados, no (M)es ou (T)odos (A/P/M/T) ? " GET ePERIODO PICT "@!X" VALID ePERIODO $("APMT")
READ
DO WHILE .T.
   eCONTA = 0
   PLQUA("01")
   @ 23,00 SAY "Informe o No. da Conta Corrente ou (0) para Todas ? " GET eCONTA PICT "@R 999"
   READ
   SELE 7
   SEEK eCONTA
   IF FOUND()
      EXIT
   ELSE
      IF eCONTA = 0
         EXIT
      ELSE
         PLQUA("01")
         PMSGR( "Atená∆o !",3)
         PMENSAG("Conta N∆o Existe !")
         PPARA()
         LOOP
      ENDIF
   ENDIF
ENDDO
IF eORDEM = "I"
   eTOTAL = .F.
   PLQUA("01") 
   PPERG ( "Deseja Total por Unidade (S/N) ? ")
   IF sisRESP = "S"
      eTOTAL = .T.
   ELSE
      eTOTAL = .F.
   ENDIF
ELSE
   eTOTAL = .F.
ENDIF
eDESISTENTE = .T.
PLQUA("01") 
PPERG ( "Deseja Pagamentos dos Desistàntes (S/N) ? ")
IF sisRESP = "S"
   eDESISTENTE = .T.
ELSE
   eDESISTENTE = .F.
ENDIF
eCLIENTE = .F.
PLQUA("01")
PPERG ( "Deseja imprimir o Nome\CPF\CNPJ do Cliente (S/N) ? ")
IF sisRESP = "S"
   eCLIENTE = .T.
ENDIF
eARQUIVO = SPACE(60)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"PAGAMENTOS.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "PAGAMENTOS.TXT"+SPACE(10) 
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
IF eQUADRA <> SPACE(5)
      SELE 8
      eIND_CLAS   = STR(eCLAS_PRO,5)
      eIND_QUAD   = eQUADRA
      eZERO = "000"
      eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 

      IF eORDEM = "O"
         USE &eARQ INDEX &eIND3,&eIND2,&eIND1
         GOTO TOP
      ELSE
         USE &eARQ INDEX &eIND1,&eIND2,&eIND3
         GOTO TOP
      ENDIF
  
      SEL_PAG3()

      SELE 8
      IF eDESISTENTE

         SEL_DESI()       

      ENDIF
      SELE 8

ELSE
   PLQUA("01")
   PMSGR( "Atená∆o !",3)
   PMENSAG("Aguarde a montagem do Banco de Dados para Impress∆o ...")
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

         SEL_PAGA()

         SELE 3
         SKIP
      ENDDO
      SELE 8
      IF eDESISTENTE

         SEL_DESI()       

      ENDIF
      SELE 8
      IF eORDEM = "O"
         USE TEMP_PAR
         INDEX ON DATA_PGTO TO TEMP_PAR
         GOTO TOP
         USE TEMP_PAR INDEX TEMP_PAR
         GOTO TOP
      ELSE
         USE TEMP_PAR
         INDEX ON CLASSE+QUADRA+NUM_IMOVEL+STR(PARTE,1) TO TEMP_PAR         
         GOTO TOP
         USE TEMP_PAR INDEX TEMP_PAR
         GOTO TOP
      ENDIF
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

         SEL_PAGA()

         SELE 4
         SKIP
      ENDDO
      SELE 8
      IF eDESISTENTE

         SEL_DESI()

      ENDIF
      SELE 8
      IF eORDEM = "O"
         USE TEMP_PAR
         INDEX ON DATA_PGTO TO TEMP_PAR
         GOTO TOP
         USE TEMP_PAR INDEX TEMP_PAR
         GOTO TOP
      ELSE
         USE TEMP_PAR
         INDEX ON CLASSE+QUADRA+NUM_IMOVEL+STR(PARTE,1) TO TEMP_PAR         
         GOTO TOP
         USE TEMP_PAR INDEX TEMP_PAR
         GOTO TOP
      ENDIF
   ENDIF
ENDIF
SELE 8
GOTO TOP
eCLASSE     = CLASSE
eQUADRA     = QUADRA
eNUM_IMOVEL = NUM_IMOVEL
ePARTE      = PARTE
IF EOF()
   PLQUA("01")
   PMSGR( "Atená∆o !",3)
   PMENSAG("N∆o H† Pagamentos Neste Per°odo !")
   PPARA()
ELSE
   * Controle de No. de Vias
   eVIAS     = 1
   eCONTADOR = 1
   PLQUA("01")
   @ 23,00 SAY "Informe a Quantidade de C¢pias do Relat¢rio ? " GET eVIAS PICT "9" 
   READ

   DO WHILE eCONTADOR <= eVIAS

   IF eSAIDA = "ARQUIVO"
      SET PRINT ON
      SET DEVICE TO PRINT
      SET PRINTER TO &eARQUIVO
   ELSE
      PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
      PPARA()
      PIMPR()
      SET DEVICE TO PRINT
   ENDIF
   eDATA_PGTO = DATA_PGTO
   eSUB_PARC  = 0
   eSUB_MULTA = 0
   eSUB_DESC  = 0
   eSUB_PGTO  = 0
   eSUB_TIT   = 0
   eTOT_PARC  = 0
   eTOT_MULTA = 0
   eTOT_DESC  = 0
   eTOT_PGTO  = 0
   eTOT_TIT   = 0
   DO WHILE .NOT. EOF()
      @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
      @ 01,115 SAY "Data : "+dtoc(DATE())
      @ 02,00  SAY "Sistema : "+sisTIT2
      @ 02,115 SAY "Relat: SAN004"
      @ 03,30  SAY sisLETRAE+"RELATORIO RECEBIMENTOS DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
      @ 04,00  SAY "Produto : "+STR(eCLAS_PRO,5)+" - "+ALLTRIM(eDESC_PRO)+IIF(eQUADRA = SPACE(5)," ",+" - Quadra\Bloco : "+ALLTRIM(eQUADRA))
      @ 05,00  SAY "Periodo : "+IIF(ePERIODO = "T","Todos",IIF(ePERIODO = "A","Atrazado",IIF(ePERIODO = "P","Postergado","do Mes")))
      @ 05,30  SAY "Origem : "+IIF(eORIGEM = "A","Ambas",IIF(eORIGEM = "C","Carteira","Bancaria"))
      @ 05,60  SAY "Tipo : "+IIF(eTIPO = "A","Todas",IIF(eTIPO = "S","Sinal",IIF(eTIPO = "I","Intermediarias","Normais")))
      @ 06,00  SAY REPLICATE("=",135)
      @ 07,02  SAY "No. IMOVEL"
      @ 07,15  SAY "DATA VENC."
      @ 07,27  SAY "TIPO"
      @ 07,34  SAY "PARC"
      @ 07,40  SAY "CONTA"
      @ 07,47  SAY "VLR. PARCELA"
      * TRAVA PARA SOLIDA E DELLY
      IF sisCGC_CPF <> "06983112000196" .OR. sisCGC_CPF <> "02852392000142" 
         @ 07,67  SAY "MULTAS/JUROS"
      ENDIF
      @ 07,87  SAY "DESCONTOS"
      @ 07,107 SAY "VLR. PAGO"
      @ 07,123 SAY "DATA PGTO."
      @ 07,133 SAY "O"
      @ 08,00 SAY REPLICATE("=",135)
      LIN = 9
      DO WHILE .NOT. EOF() .AND. LIN <= 54
         IF eCLIENTE
            SELE 8
            eCLASSE     = CLASSE
            eQUADRA     = QUADRA
            eNUM_IMOVEL = NUM_IMOVEL
            ePARTE      = PARTE
            SELE 6
            GOTO TOP
            SEEK eCLASSE+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
            eNUM_CLIE = NUM_CLIE
            eDATA_VENDA = DATA_VENDA            
            SELE 5
            GOTO TOP
            SEEK eNUM_CLIE
            eNOME = CLIENTE
            eCPF_CGC = CPF_CGC
            SELE 8
            @ LIN,02  SAY ALLTRIM(eNOME)+" - CPF\CNPJ : "+ALLTRIM(eCPF_CGC)+" - DATA VENDA : "+DTOC(eDATA_VENDA)
            LIN = LIN + 1
         ENDIF
         @ LIN,02  SAY ALLTRIM(CLASSE)+"/"+ALLTRIM(QUADRA)+"/"+ALLTRIM(NUM_IMOVEL)+"/"+STR(PARTE,1)
         @ LIN,15  SAY DATA_VENC
         @ LIN,29  SAY TIPO_PARC
         @ LIN,35  SAY ALLTRIM(NUM_PARC)
         @ LIN,40  SAY CONTA      PICT "@R 999" 
         @ LIN,48  SAY VLR_PARC   PICT "@R 999,999,999.99"
         eDESCONTO = 0
         eMULTA    = 0
         IF VLR_PGTO <> VLR_PARC
            IF VLR_PGTO > VLR_PARC
               eMULTA    = VLR_PGTO - VLR_PARC
            ELSE
               eDESCONTO = VLR_PARC - VLR_PGTO
            ENDIF
         ENDIF
         * TRAVA PARA SOLIDA E DELLY
         IF sisCGC_CPF <> "06983112000196" .OR. sisCGC_CPF <> "02852392000142" 
            @ LIN,64  SAY eMULTA     PICT "@R 999,999,999,999.99"
         ENDIF
         @ LIN,84  SAY eDESCONTO  PICT "@R 999,999,999,999.99"
         @ LIN,104 SAY VLR_PGTO   PICT "@R 999,999,999,999.99"
         @ LIN,123 SAY DATA_PGTO
         @ LIN,133 SAY ALLTRIM(ORIGEM) PICT "X"
         IF eORIGEM = "C"
            LIN = LIN + 1
            @ LIN,70 SAY "Boleto/Banco/Ag/C.C/Cheque: "+ALLTRIM(COD_BANCO)+"/"+NUM_BANCO+"/"+ALLTRIM(NUM_AGENC)+"/"+ALLTRIM(NUM_CONTA)+"/"+ALLTRIM(NUM_CHEQUE)
         ENDIF
         eSUB_PARC  = eSUB_PARC  + VLR_PARC
         eSUB_MULTA = eSUB_MULTA + eMULTA
         eSUB_DESC  = eSUB_DESC  + eDESCONTO
         eSUB_PGTO  = eSUB_PGTO  + VLR_PGTO
         eSUB_TIT   = eSUB_TIT + 1
         LIN = LIN + 1
         SKIP
         IF eTOTAL
            IF eQUADRA = QUADRA .AND. eNUM_IMOVEL = NUM_IMOVEL .AND. ePARTE = PARTE

            ELSE
               eCLASSE     = CLASSE
               eQUADRA     = QUADRA
               eNUM_IMOVEL = NUM_IMOVEL
               ePARTE      = PARTE
               @ LIN,02  SAY "SubTotal    : "
               @ LIN,44  SAY eSUB_PARC   PICT "@R 999,999,999,999.99"
               * TRAVA PARA SOLIDA E DELLY
               IF sisCGC_CPF <> "06983112000196" .OR. sisCGC_CPF <> "02852392000142" 
                  @ LIN,64  SAY eSUB_MULTA  PICT "@R 999,999,999,999.99"
               ENDIF
               @ LIN,84  SAY eSUB_DESC   PICT "@R 999,999,999,999.99"
               @ LIN,104 SAY eSUB_PGTO   PICT "@R 999,999,999,999.99"
               LIN = LIN + 2
               eTOT_PARC  = eTOT_PARC  + eSUB_PARC
               eTOT_MULTA = eTOT_MULTA + eSUB_MULTA
               eTOT_DESC  = eTOT_DESC  + eSUB_DESC
               eTOT_PGTO  = eTOT_PGTO  + eSUB_PGTO
               eTOT_TIT   = eTOT_TIT   + eSUB_TIT
               eSUB_PARC  = 0
               eSUB_MULTA = 0
               eSUB_DESC  = 0
               eSUB_PGTO  = 0
               eSUB_TIT   = 0
            ENDIF
         ELSE
            eTOT_PARC  = eTOT_PARC  + eSUB_PARC
            eTOT_MULTA = eTOT_MULTA + eSUB_MULTA
            eTOT_DESC  = eTOT_DESC  + eSUB_DESC
            eTOT_PGTO  = eTOT_PGTO  + eSUB_PGTO
            eTOT_TIT   = eTOT_TIT   + eSUB_TIT
            eSUB_PARC  = 0
            eSUB_MULTA = 0
            eSUB_DESC  = 0
            eSUB_PGTO  = 0
            eSUB_TIT   = 0
         ENDIF
         PTECLA()
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
         @ LIN,00  SAY REPLICATE("-",135)
         LIN = LIN + 1
         @ LIN,02  SAY "Total GERAL : "
         @ LIN,35  SAY eTOT_TIT    PICT "@R 999,999"
         @ LIN,44  SAY eTOT_PARC   PICT "@R 999,999,999,999.99"
         * TRAVA PARA SOLIDA E DELLY
         IF sisCGC_CPF <> "06983112000196" .OR. sisCGC_CPF <> "02852392000142" 
            @ LIN,64  SAY eTOT_MULTA  PICT "@R 999,999,999,999.99"
         ENDIF
         @ LIN,84  SAY eTOT_DESC   PICT "@R 999,999,999,999.99"
         @ LIN,104 SAY eTOT_PGTO   PICT "@R 999,999,999,999.99"
      ENDIF
      @ 56,00 SAY REPLICATE("=",135)
      @ 57,95 SAY sisTIT3
      @ 01,00 SAY " "+sisLETRAN
   ENDDO
   IF eSAIDA = "ARQUIVO"
      SET PRINT OFF
      SET DEVICE TO SCREEN
   ELSE
      SET DEVICE TO SCREEN
   ENDIF
   * Controle de numero de vias
   eCONTADOR = eCONTADOR + 1
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
FUNCTION SEL_PAGA()
PLQUA("01")
PMENSAG("Aguarde o Processamento ...")
DO CASE
CASE ePERIODO = "T"
   DO CASE
      CASE eTIPO = "A"
         IF eORIGEM = "A"
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE 
            IF eORIGEM = "C"
               APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ORIGEM = "B" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ORIGEM = " " .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "N"
         IF eORIGEM = "A"
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
             .AND. ORIGEM = "B" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
             .AND. ORIGEM = " " .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "I" 
         IF eORIGEM = "A"
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = "B" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = " " .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "S" 
         IF eORIGEM = "A"
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = "B" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = " " .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
   ENDCASE
CASE ePERIODO = "A"
   DO CASE
      CASE eTIPO = "A"
         IF eORIGEM = "A"
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE 
            IF eORIGEM = "C"
               APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "N"
         IF eORIGEM = "A"
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
             .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
             .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "I" 
         IF eORIGEM = "A"
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "S" 
         IF eORIGEM = "A"
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
   ENDCASE
CASE ePERIODO = "P"
DO CASE
   CASE eTIPO = "A"
      IF eORIGEM = "A"
         APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE 
         IF eORIGEM = "C"
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   CASE eTIPO = "N"
      IF eORIGEM = "A"
         APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE
         IF eORIGEM = "C"
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
          .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
          .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   CASE eTIPO = "I" 
      IF eORIGEM = "A"
         APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE
         IF eORIGEM = "C"
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   CASE eTIPO = "S" 
      IF eORIGEM = "A"
         APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE
         IF eORIGEM = "C"
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   ENDCASE
CASE ePERIODO = "M"
DO CASE
   CASE eTIPO = "A"
      IF eORIGEM = "A"
         APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE 
         IF eORIGEM = "C"
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   CASE eTIPO = "N"
      IF eORIGEM = "A"
         APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE
         IF eORIGEM = "C"
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
          .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
          .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   CASE eTIPO = "I" 
      IF eORIGEM = "A"
         APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE
         IF eORIGEM = "C"
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   CASE eTIPO = "S" 
      IF eORIGEM = "A"
         APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE
         IF eORIGEM = "C"
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   ENDCASE
ENDCASE
PLQUA("01")
RETURN nil
*************************  fim  *********************************
* PESQUISA DE PARCELAS DE DESISTENTES
FUNCTION SEL_DESI()		
PLQUA("01")
PMENSAG("Aguarde o Processamento ...")
eFILTRO = "CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0)"
DO CASE
CASE ePERIODO = "T"
DO CASE
   CASE eTIPO = "A"
      IF eORIGEM = "A"
         APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE 
         IF eORIGEM = "C"
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = "B" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = " " .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   CASE eTIPO = "N"
      IF eORIGEM = "A"
         APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE
         IF eORIGEM = "C"
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
          .AND. ORIGEM = "B" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
          .AND. ORIGEM = " " .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   CASE eTIPO = "I" 
      IF eORIGEM = "A"
         APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE
         IF eORIGEM = "C"
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = "B" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = " " .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   CASE eTIPO = "S" 
      IF eORIGEM = "A"
         APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE
         IF eORIGEM = "C"
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = "B" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = " " .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   ENDCASE
CASE ePERIODO = "A"
DO CASE
   CASE eTIPO = "A"
      IF eORIGEM = "A"
         APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE 
         IF eORIGEM = "C"
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   CASE eTIPO = "N"
      IF eORIGEM = "A"
         APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE
         IF eORIGEM = "C"
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
          .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
          .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   CASE eTIPO = "I" 
      IF eORIGEM = "A"
         APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE
         IF eORIGEM = "C"
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   CASE eTIPO = "S" 
      IF eORIGEM = "A"
         APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE
         IF eORIGEM = "C"
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   ENDCASE
CASE ePERIODO = "P"
DO CASE
   CASE eTIPO = "A"
      IF eORIGEM = "A"
         APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE 
         IF eORIGEM = "C"
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000) 
         ENDIF
      ENDIF
   CASE eTIPO = "N"
      IF eORIGEM = "A"
         APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE
         IF eORIGEM = "C"
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
          .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
          .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   CASE eTIPO = "I" 
      IF eORIGEM = "A"
         APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE
         IF eORIGEM = "C"
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   CASE eTIPO = "S" 
      IF eORIGEM = "A"
         APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE
         IF eORIGEM = "C"
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   ENDCASE
CASE ePERIODO = "M"
DO CASE
   CASE eTIPO = "A"
      IF eORIGEM = "A"
         APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE 
         IF eORIGEM = "C"
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   CASE eTIPO = "N"
      IF eORIGEM = "A"
         APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE
         IF eORIGEM = "C"
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
          .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
          .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   CASE eTIPO = "I" 
      IF eORIGEM = "A"
         APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE
         IF eORIGEM = "C"
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   CASE eTIPO = "S" 
      IF eORIGEM = "A"
         APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
      ELSE
         IF eORIGEM = "C"
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            APPEND ALL FROM CAD_QUIT FOR CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ENDIF
      ENDIF
   ENDCASE
ENDCASE
PLQUA("01")
RETURN nil
*************************  fim  *********************************
FUNCTION SEL_PAG3()
PLQUA("01")
PMENSAG("Aguarde o Processamento ...")
DO CASE
CASE eNUM_IMOVEL <> SPACE(5)
   eFILTRO = "CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0)"
   DO CASE
   CASE ePERIODO = "T"
      DO CASE
      CASE eTIPO = "A"
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE 
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = "B" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = " " .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "N"
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. CONTABIL  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
               .AND. ORIGEM = "B" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
               .AND. ORIGEM = " " .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "I" 
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = "B" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = " " .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "S" 
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = "B" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = " " .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      ENDCASE
   CASE ePERIODO = "A"
      DO CASE
      CASE eTIPO = "A"
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE 
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "N"
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
               .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
               .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "I" 
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "S" 
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      ENDCASE
   CASE ePERIODO = "P"
      DO CASE
      CASE eTIPO = "A"
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE 
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "N"
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
               .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
               .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "I" 
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0).AND.SUBSTR(TIPO_PARC,1,1)="I".AND.ORIGEM="B".AND.CONTABIL.AND.(DATA_VENC>eDATA_FIM).AND.IIF(eCONTA<>0,CONTA=eCONTA,CONTA<>1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0).AND.SUBSTR(TIPO_PARC,1,1)="I".AND.ORIGEM="".AND.CONTABIL.AND.(DATA_VENC>eDATA_FIM).AND.IIF(eCONTA<>0,CONTA=eCONTA,CONTA<>1000)
            ENDIF
         ENDIF
      CASE eTIPO = "S" 
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      ENDCASE
   CASE ePERIODO = "M"
      DO CASE
      CASE eTIPO = "A"
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE 
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "N"
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
               .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
               .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "I" 
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "S" 
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.eNUM_IMOVEL=NUM_IMOVEL.AND.ePARTE =PARTE.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      ENDCASE
   ENDCASE
CASE eNUM_IMOVEL = SPACE(5)
   eFILTRO = "CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0)"
   DO CASE
   CASE ePERIODO = "T"
      DO CASE
      CASE eTIPO = "A"
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE 
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = "B" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = " " .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "N"
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0)   .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. CONTABIL  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0)   .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
               .AND. ORIGEM = "B" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0)   .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
               .AND. ORIGEM = " " .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "I" 
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = "B" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = " " .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "S" 
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = "B" .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = " " .AND. CONTABIL .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      ENDCASE
   CASE ePERIODO = "A"
      DO CASE
      CASE eTIPO = "A"
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE 
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "N"
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
               .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
               .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "I" 
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "S" 
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC < eDATA_INI)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      ENDCASE
   CASE ePERIODO = "P"
      DO CASE
      CASE eTIPO = "A"
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE 
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "N"
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
               .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
               .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "I" 
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0).AND.SUBSTR(TIPO_PARC,1,1)="I".AND.ORIGEM="B".AND.CONTABIL.AND.(DATA_VENC>eDATA_FIM).AND.IIF(eCONTA<>0,CONTA=eCONTA,CONTA<>1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0).AND.SUBSTR(TIPO_PARC,1,1)="I".AND.ORIGEM="".AND.CONTABIL.AND.(DATA_VENC>eDATA_FIM).AND.IIF(eCONTA<>0,CONTA=eCONTA,CONTA<>1000)
            ENDIF
         ENDIF
      CASE eTIPO = "S" 
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC > eDATA_FIM)  .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      ENDCASE
   CASE ePERIODO = "M"
      DO CASE
      CASE eTIPO = "A"
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE 
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "N"
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
               .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R");
               .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "I" 
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "I" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      CASE eTIPO = "S" 
         IF eORIGEM = "A"
            SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
         ELSE
            IF eORIGEM = "C"
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = "B" .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ELSE
               SET FILTER TO CLASSE=eIND_CLAS.AND.QUADRA=eIND_QUAD.AND.(DATA_PGTO>=eDATA_INI.AND.DATA_PGTO<=eDATA_FIM.AND.VLR_PGTO<>0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" .AND. ORIGEM = " " .AND. CONTABIL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
            ENDIF
         ENDIF
      ENDCASE
   ENDCASE
ENDCASE
GOTO TOP
RETURN nil
*************************  fim  *********************************
