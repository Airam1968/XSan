*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CORRECAO.PRG
* Data        : 01/01/99
* Objetivo    : CORRECAO MONETARIA DOS FINANCIAMENTOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CORRECAO()
SAVE SCREEN TO TELA03
PTIT( "CORRECAO MONETµRIA DE FINANCIAMENTO")
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
USE TAXA_MES INDEX TAXA_MES
GOTO TOP
SELE 8
USE STATUS
GOTO TOP
eCORR_PGTO = CORR_PGTO
eRESI_I    = RESIDUO_I
eRESI_SEPA = RESI_SEPA
eCORR_BOLE = CORR_BOLE
eTIPO_PARC = " "
eFAZ       = .T.
eFAZ2      = .T.
USE
* Cadastro de Parcelas
SELE 9
USE INDICES INDEX INDICES
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
eQUADRA  = SPACE(1)
DO CASE
   CASE eTIPO_PRO = 1
      * Consulta de Loteamentos
      SELE 3
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA     = SPACE(5)
      eNUM_IMOVEL = SPACE(5)
      eSTATUS     = 0
      ePROPR_1    = 0
      ePROPR_2    = 0
      ePARTE      = 0
      PLQUA("01")
      PPERG ( "Deseja Selecionar a "+ALLTRIM(sisGRUPO1)+" (S/N) ? ")
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
         DBEDIT(6,14,6,75,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
         eQUADRA     = NUM_QUADRA
         eNUM_INICIO = NUM_INICIO
         eNUM_FINAL  = NUM_FINAL
         eNUM_IMOVEL = SPACE(5)
         PLQUA("01")
         PPERG ( "Deseja Selecionar "+ALLTRIM(sisUNID1)+" (S/N) ? ")
         IF sisRESP = "S"
            @ 07,02 SAY sisUNID1+" : "
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
            GOTO TOP
            IF EOF()
               PLQUA("01")
	       PMSGR( "Atená∆o !",3)
	       PMENSAG("N∆o H† Unidades Dispon°veis !")
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
            ePARTE      = STATUS
            IF ePARTE <> 0
               eOPCAO = 0
               PLQUA("01")
               @ 23,00 SAY "Informe Correá∆o Para o (1)o. ou (2)o. Meio Lote ? " GET eOPCAO PICT "99999" RANGE 1,2
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
      PPERG ( "Deseja Selecionar o "+ALLTRIM(sisGRUPO2)+" (S/N) ? ")
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
         DBEDIT(6,08,6,76,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
         eQUADRA      = NUM_BLOCO
         eQUANTIDADE = QUANTIDADE
         eNUM_IMOVEL = SPACE(5)
         PLQUA("01")
         PPERG ( "Deseja Selecionar "+ALLTRIM(sisUNID2)+" (S/N) ? ")
         IF sisRESP = "S"
            @ 07,02 SAY sisUNID2+" : "
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
            GOTO TOP
            IF EOF()
               PLQUA("01")
	       PMSGR( "Atená∆o !",3)
	       PMENSAG("N∆o H† Unidades Dispon°veis !")
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
            ePARTE      = STATUS
         ENDIF
      ENDIF
   CASE eTIPO_PRO = 3
      * Consulta de Salas Comerciais
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA    = SPACE(5)
      eNUM_IMOVEL = SPACE(5)
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
         DBEDIT(6,10,6,41,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
         eQUADRA     = NUM_BLOCO
         eQUANTIDADE = QUANTIDADE
         eNUM_IMOVEL = SPACE(5)
         PLQUA("01")
         PPERG ( "Deseja Selecionar o Apartamento (S/N) ? ")
         IF sisRESP = "S"
            @ 07,02 SAY "Apto.   : "
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
            GOTO TOP
            IF EOF()
               PLQUA("01")
	       PMSGR( "Atená∆o !",3)
	       PMENSAG("N∆o H† Unidades Dispon°veis !")
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
            ePARTE      = STATUS
         ENDIF
      ENDIF
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
eDATA_INI   = CTOD("01/01/1980")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   @ 08,02 SAY "Correá∆o atÇ: " GET eDATA_FIM
   READ
   IF eDATA_INI = CTOD("") .OR. eDATA_FIM = CTOD("")
      PLQUA("01")
      PMSGR( "Datas Inv†lidas !",3)
      LOOP
   ELSE
      EXIT
   ENDIF
ENDDO
PLQUA("01")
PPERG ( "Confirma a Correá∆o Monet†ria (S/N) ? ")
IF sisRESP = "N"
   CLOSE DATABASES
   RESTORE SCREEN FROM TELA03
   RETURN nil
ENDIF
PLQUA("01")
PPERG ( "Preparaá∆o da Planilha (S/N) ? ")
IF sisRESP = "S"
   ePLANILHA = .F.
ELSE
   ePLANILHA = .T.
ENDIF
IF eQUADRA = SPACE(5)
   SELE 6
   SET FILTER TO CLASSE = STR(eCLAS_PRO,5) 
   GOTO TOP
ELSE
   IF eNUM_IMOVEL = SPACE(5)
      SELE 6
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA
      GOTO TOP
   ELSE
      SELE 6
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE
      GOTO TOP
   ENDIF
ENDIF
DO WHILE .NOT. EOF()
   eNUM_IMOVEL = NUM_IMOVEL
   eQUADRA     = QUADRA
   ePARTE      = PARTE      
   eDT_VENDA   = ULT_CORREC
   eINDICE     = INDICE
   ePARCELAS   = PARCELAS
   ePERIODO    = PERIODO
   eULT_CORREC = ULT_CORREC
   SELE 8
   eIND_CLAS = STR(eCLAS_PRO,5)
   eIND_QUAD = eQUADRA
   eZERO = "000"
   eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
   eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
   eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
   eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
   USE &eARQ INDEX &eIND2,&eIND1,&eIND3
   GOTO TOP
   SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE 
   GOTO TOP
   PLQUA("01")
   PMENSAG("Imovel : "+STR(eCLAS_PRO,5)+"/"+eQUADRA+"/"+eNUM_IMOVEL+"/"+STR(ePARTE,1))
   IF EOF() .OR. ALLTRIM(eINDICE) = "NENHUM"
      PLQUA("01")
      PMENSAG("Atená∆o !")
      PMENSAG("N∆o Ha Parcelas a Serem Corrigidas !")
      SELE 6
      SKIP
      LOOP
   ELSE
      * Correcao Mensal de Contratos
      DO CASE
         CASE ePERIODO = "MENSAL"

            DO COR_MENSAL

         CASE ePERIODO = "ANUAL"

            DO COR_ANUAL

         CASE ePERIODO = "ANUAL12"

            DO COR_ANUAL12

         CASE ePERIODO = "SEMESTRAL"

            DO COR_SEMESTRAL

         OTHERWISE
            PLQUA("01")
            PMSGR( "Atená∆o !",3)
            PMENSAG("Erro Grave, Falta a Periodicidade, informe o suporte...")
            PPARA()
            SELE 6
            SKIP
            LOOP
      ENDCASE
   ENDIF
ENDDO
PLQUA("01")
PMSGR( "Processamento Conclu°do !",3)
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
FUNCTION COR_MENSAL()
            PLQUA("01")
	    PMENSAG("Mensal ...")
            SELE 8
            GOTO TOP
            DO WHILE .NOT. EOF()
               PLQUA("01")
	       PMENSAG("Efetuando Correá∆o das Parcelas do Im¢vel ("+ALLTRIM(STR(eCLAS_PRO,5))+"/"+ALLTRIM(eQUADRA)+"/"+ALLTRIM(eNUM_IMOVEL)+"/"+STR(ePARTE,1)+") Parcela No. "+NUM_PARC)
               eMES_LIMIT = SUBSTR(DTOC(eDATA_FIM),4,2)
               eANO_LIMIT = SUBSTR(DTOC(eDATA_FIM),7,4)
               eVLR_ORIG  = VLR_PARC
               eDATA_VENC = DATA_VENC
               eCOD_BANCO = COD_BANCO
               eMES_PARCE = SUBSTR(DTOC(eDATA_VENC),4,2)
               eANO_PARCE = SUBSTR(DTOC(eDATA_VENC),7,4)
               eMES_VENDA = SUBSTR(DTOC(eDT_VENDA),4,2)
               eANO_VENDA = SUBSTR(DTOC(eDT_VENDA),7,4)
               SELE 7         
               GOTO TOP
               SET FILTER TO ((ANO+MES >= eANO_VENDA+eMES_VENDA) .AND. (ANO+MES <= eANO_PARCE+eMES_PARCE)) .AND. (ANO+MES <= eANO_LIMIT+eMES_LIMIT)
               GOTO TOP
               eIND_ACUM  = 1
               eIND_ANTE  = 0
               DO WHILE (ANO+MES < eANO_PARCE+eMES_PARCE) .AND. .NOT. EOF()
                  eIND_ACUM = ((&eINDICE/100) + 1) * eIND_ACUM
                  SKIP
               ENDDO
               SKIP -1
               eIND_ANTE = ((&eINDICE/100) + 1)
               SELE 8
               IF ATIVA
                  IF F_BLOQR()
                     IF eCORR_BOLE
                        IF eCORR_PGTO
                           REPLACE VLR_PARC  WITH VLR_ORIGIN * eIND_ACUM
                           REPLACE INDICE    WITH eINDICE
                           REPLACE TAXA_MES  WITH eIND_ANTE
                           REPLACE TAXA_ACUM WITH ((eIND_ACUM - 1) * 100)
                           REPLACE VLR_BASE  WITH VLR_ORIGIN
                        ELSE
                           IF VLR_PGTO = 0
                              REPLACE VLR_PARC WITH VLR_ORIGIN * eIND_ACUM
                              REPLACE INDICE    WITH eINDICE
                              REPLACE TAXA_MES  WITH eIND_ANTE
                              REPLACE TAXA_ACUM WITH ((eIND_ACUM - 1) * 100)
                              REPLACE VLR_BASE  WITH VLR_ORIGIN
                           ENDIF
                        ENDIF
                     ENDIF
                     UNLOCK
                  ENDIF
               ENDIF
               SELE 8
               SKIP
            ENDDO
            SELE 6
            SKIP

RETURN nil
*************************  fim  *********************************
FUNCTION COR_ANUAL()
            * Correcao Anual de Contratos
            PLQUA("01")
	    PMENSAG("Anual ...")
            SELE 6
            IF ULT_CORREC < CTOD("01/"+SUBSTR(DTOC(eDATA_FIM),4,2)+"/"+STR((VAL(SUBSTR(DTOC(eDATA_FIM),7,4))-1),4))
               SELE 8
               GOTO TOP
               PLQUA("01")
	       PMENSAG("Preparando Parcelas ...")
               eQUANT_PAR = 1
               DO WHILE .NOT. EOF()
                  SELE 8
                  IF DATA_VENC >= CTOD("01/"+SUBSTR(DTOC(eDATA_FIM),4,2)+"/"+SUBSTR(DTOC(eDATA_FIM),7,4)) .AND. VLR_PGTO = 0
                     eQUANT_PAR = eQUANT_PAR + 1
                  ENDIF
                  IF ePLANILHA  &&&
                     IF F_BLOQR()
                        REPLACE VLR_ORIGIN WITH VLR_PARC
                        UNLOCK
                     ENDIF
                  ENDIF   &&&
                  SKIP
               ENDDO
               SELE 8
               GOTO TOP
               eACUM_RES1 = 0
               eACUM_TAXA = 1
               eACUM_RESA = 0
               DO WHILE .NOT. EOF()
                  PLQUA("01")
		  PMENSAG("Efetuando Correá∆o das Parcelas do Im¢vel ("+ALLTRIM(STR(eCLAS_PRO,5))+"/"+ALLTRIM(eQUADRA)+"/"+ALLTRIM(eNUM_IMOVEL)+"/"+STR(ePARTE,1)+") Parcela No. "+NUM_PARC)
		  *
                  eMES_LIMIT = STR(IIF((VAL(SUBSTR(DTOC(eDATA_FIM),4,2))-2) <= 0,IIF((VAL(SUBSTR(DTOC(eDATA_FIM),4,2))-2) = 0,12,11),(VAL(SUBSTR(DTOC(eDATA_FIM),4,2))-2)),2)
                  eANO_LIMIT = STR(IIF((VAL(SUBSTR(DTOC(eDATA_FIM),4,2))-2) <= 0,VAL(SUBSTR(DTOC(eDATA_FIM),7,4))-1,VAL(SUBSTR(DTOC(eDATA_FIM),7,4))),4)
                  eMES_LIMIT = IIF(LEN(ALLTRIM(eMES_LIMIT)) = 1,"0"+ALLTRIM(eMES_LIMIT),ALLTRIM(eMES_LIMIT))
                  eVLR_ORIG  = VLR_PARC
                  eDATA_VENC = DATA_VENC
                  eCOD_BANCO = COD_BANCO
                  *
                  eMES_PARCE = SUBSTR(DTOC(eDATA_VENC),4,2)
                  eANO_PARCE = SUBSTR(DTOC(eDATA_VENC),7,4)
                  eMES_VENDA = STR(IIF((VAL(SUBSTR(DTOC(eDATA_FIM),4,2))-1) <= 0,12,(VAL(SUBSTR(DTOC(eDATA_FIM),4,2))-1)),2)
                  eMES_VENDA = IIF(LEN(ALLTRIM(eMES_VENDA)) = 1,"0"+ALLTRIM(eMES_VENDA),ALLTRIM(eMES_VENDA))
                  eANO_VENDA = IIF((VAL(SUBSTR(DTOC(eDATA_FIM),4,2))-1) <= 0,ALLTRIM(STR((VAL(SUBSTR(DTOC(eDATA_FIM),7,4))-2),4)),ALLTRIM(STR((VAL(SUBSTR(DTOC(eDATA_FIM),7,4))-1),4)))
                  SELE 7
                  SET FILTER TO ((ANO+MES >= eANO_VENDA+eMES_VENDA) .AND. (ANO+MES <= eANO_LIMIT+eMES_LIMIT))
                  GOTO TOP
                  SELE 7         
                  GOTO TOP
                  eIND_ACUM  = 1
		*? "mes atual : "+ano+mes
		*?? "parc : "+Eano_parce+Emes_parce+" "
		*?? "venda: "+Eano_venda+Emes_venda+" "
		*?? "Limit: "+Eano_limit+Emes_limit+" "
		*?? "indice: "+Eindice
		*do ppara
                  DO WHILE (ANO+MES <= eANO_PARCE+eMES_PARCE) .AND. .NOT. EOF()
                     eIND_ACUM =  eIND_ACUM * ((&eINDICE/100) + 1)
                     SKIP
                  ENDDO
                  SELE 8
                  @ 12,00 SAY "Valor Original : " GET VLR_ORIGIN PICT "@R 999,999,999.99"
                  @ 13,00 SAY "Valor Parcela  : " GET VLR_PARC   PICT "@R 999,999,999.99"
                  @ 14,00 SAY "Indice Acum.   : " GET eIND_ACUM  PICT "@R   999,999.9999"
                  @ 15,00 SAY "Residuo Acum.  : " GET eACUM_RES1 PICT "@R 999,999,999.99"
                  eULT_INDI  = 0
                  eULT_IND2  = 0
                  *
                  IF DATA_VENC >= CTOD("01/"+eMES_VENDA+"/"+eANO_VENDA) .AND. DATA_VENC <= CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT)
                     SELE 7
                     SET FILTER TO ((ANO+MES >= eANO_VENDA+eMES_VENDA) .AND. (ANO+MES <= eANO_LIMIT+eMES_LIMIT))
                     GOTO TOP
                     eIND_RESI  = 1
                     eULT_INDI  = 0
                     eULT_IND2  = 0
                     DO WHILE (ANO+MES < eANO_PARCE+eMES_PARCE) .AND. .NOT. EOF()
                        eIND_RESI = eIND_RESI * ((&eINDICE/100) + 1)
                        SKIP 
                     ENDDO
                     eULT_IND2 = &eINDICE
                     SKIP - 1
                     eULT_INDI = &eINDICE
                     SELE 8
                     eACUM_RESA = eACUM_RES1
                     *
                     IF DATA_VENC > IIF(eMES_VENDA = "02",CTOD("28/"+eMES_VENDA+"/"+eANO_VENDA),CTOD("30/"+eMES_VENDA+"/"+eANO_VENDA)) && Opcao para fevereiro.
                        IF eRESI_I
* COM TAXA DE RESIDUO      eACUM_RES1 = (eACUM_RES1 + (((VLR_ORIGIN * ((eIND_ACUM*(1/((eULT_IND2/100)+1))))) - VLR_ORIGIN) * eIND_RESI ))
                           eACUM_RES1 = (eACUM_RES1 + (((VLR_ORIGIN * ((eIND_ACUM*(1/((eULT_IND2/100)+1))))) - VLR_ORIGIN) ))
                           @ 15,00 SAY "Residuo Acum.  : " GET eACUM_RES1 PICT "@R 999,999,999.99"
                        ELSE
*                          eACUM_RES1 = (eACUM_RES1 + ((((VLR_ORIGIN * eIND_ACUM) - VLR_ORIGIN) / eQUANT_PAR) * eIND_RESI))
                           eACUM_RES1 = (eACUM_RES1 + ((((VLR_ORIGIN * eIND_ACUM) - VLR_ORIGIN)) * eIND_RESI))
                           @ 15,00 SAY "Residuo Acum.  : " GET eACUM_RES1 PICT "@R 999,999,999.99"
                        ENDIF
                     ENDIF
                  ENDIF
                  SELE 8
                  @ 12,00 SAY "Valor Original : " GET VLR_ORIGIN PICT "@R 999,999,999.99"
                  @ 13,00 SAY "Valor Parcela  : " GET VLR_PARC   PICT "@R 999,999,999.99"
                  @ 14,00 SAY "Indice Acum.   : " GET eIND_ACUM  PICT "@R   999,999.9999"
                  @ 15,00 SAY "Residuo Acum.  : " GET eACUM_RES1 PICT "@R 999,999,999.99"
                  IF eRESI_I
                     eVLR_ATUAL = (VLR_ORIGIN * eIND_ACUM)
                  ELSE
                     eVLR_ATUAL = (VLR_ORIGIN * eIND_ACUM) + (eACUM_RES1/eQUANT_PAR)
                  ENDIF
                  @ 17,00 SAY "Valor Corrigido: " GET eVLR_ATUAL PICT "@R 999,999,999.99"
                  SELE 8
                  IF F_BLOQR()
                     IF eRESI_I
                        IF DATA_VENC > IIF(eMES_LIMIT = "02",CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT),CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT)) && Opcao para fevereiro.
                           IF eCORR_BOLE
                              IF eCORR_PGTO
                                 IF eFAZ
                                    IF ePLANILHA   &&&
                                       IF eRESI_SEPA
                                          IF TIPO_PARC <> "R99"
                                             REPLACE VLR_PARC WITH (VLR_ORIGIN * eIND_ACUM)
                                          ENDIF
                                          * Criaá∆o da Parcela de Res°duo
                                          eRESI_SEPA = .F.
                                          eREGISTRO = RECNO()
                                          eCLASSE = CLASSE
                                          eQUADRA = QUADRA
                                          eNUM_IMOVEL = NUM_IMOVEL
                                          ePARTE      = PARTE
                                          eNUM_PARC   = "    0"
                                          eTIPO_PARC  = "R99"
                                          eDATA_VENC  = DATA_VENC
                                          eVLR_PARC2  = eACUM_RES1
                                          eOBS        = "PARC.RESIDUO"
                                          eVLR_ORIGIN = eACUM_RES1
                                          eATIVA      = .T.
                                          eCONTABIL   = .T.
                                          eMULTA      = MULTA
                                          eCOMISSAO   = COMISSAO
                                          ePERID_COMI = "a.m."
                                          GOTO TOP
                                          APPEND BLANK
                                          IF F_BLOQR()
                                             REPLACE CLASSE      WITH eCLASSE,;
                                                     QUADRA      WITH eQUADRA,;
                                                     NUM_IMOVEL  WITH eNUM_IMOVEL
                                             REPLACE PARTE       WITH ePARTE,;
                                                     NUM_PARC    WITH eNUM_PARC,;
                                                     TIPO_PARC   WITH eTIPO_PARC
                                             REPLACE DATA_VENC   WITH eDATA_VENC,;
                                                     VLR_PARC    WITH eVLR_PARC2,;
                                                     OBS         WITH eOBS
                                             REPLACE VLR_ORIGIN  WITH eVLR_ORIGIN,;
                                                     ATIVA       WITH eATIVA,;
                                                     CONTABIL    WITH eCONTABIL
                                             REPLACE MULTA       WITH eMULTA,;
                                                     COMISSAO    WITH eCOMISSAO,;
                                                     PERID_COMI  WITH ePERID_COMI
                                             UNLOCK
                                          ENDIF
                                          GOTO eREGISTRO
                                          F_BLOQR()
                                       ELSE
                                         IF DATA_VENC > IIF(eMES_LIMIT = "02",CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT),CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT)) .AND. TIPO_PARC <> "R99" && Opcao para fevereiro.
                                             REPLACE VLR_PARC WITH (VLR_ORIGIN * eIND_ACUM)
                                          ENDIF
                                       ENDIF
                                       IF eFAZ2
                                          IF SUBSTR(DTOC(DATA_VENC),4,2) = eMES_VENDA
                                             * Informaá‰es da Planilha de Correcao
                                             REPLACE INDICE    WITH eINDICE
                                             REPLACE TAXA_MES  WITH 0
                                             REPLACE TAXA_ACUM WITH 0
                                             REPLACE RESI_MES  WITH 0
                                             REPLACE RESI_ACUM WITH 0
                                             REPLACE VLR_BASE  WITH VLR_ORIGIN
                                             *
                                          ELSE
                                             * Informaá‰es da Planilha de Correcao
                                             REPLACE INDICE    WITH eINDICE
                                             REPLACE TAXA_MES  WITH eULT_INDI
                                             REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                             REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                             eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                             REPLACE RESI_ACUM WITH eACUM_RES1
                                             REPLACE VLR_BASE  WITH VLR_ORIGIN
                                          ENDIF
                                          eFAZ2 = .F.
                                       ELSE
                                          * Informaá‰es da Planilha de Correcao
                                          REPLACE INDICE    WITH eINDICE
                                          REPLACE TAXA_MES  WITH eULT_INDI
                                          REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                          REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                          eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                          REPLACE RESI_ACUM WITH eACUM_RES1
                                          REPLACE VLR_BASE  WITH VLR_ORIGIN
                                       ENDIF
                                    ELSE
                                       IF eFAZ2
                                          IF SUBSTR(DTOC(DATA_VENC),4,2) = eMES_VENDA
                                             * Informaá‰es da Planilha de Correcao
                                             REPLACE INDICE    WITH eINDICE
                                             REPLACE TAXA_MES  WITH 0
                                             REPLACE TAXA_ACUM WITH 0
                                             REPLACE RESI_MES  WITH 0
                                             REPLACE RESI_ACUM WITH 0
                                             REPLACE VLR_BASE  WITH VLR_ORIGIN
                                          ELSE
                                             * Informaá‰es da Planilha de Correcao
                                             REPLACE INDICE    WITH eINDICE
                                             REPLACE TAXA_MES  WITH eULT_INDI
                                             REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                             REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                             eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                             REPLACE RESI_ACUM WITH eACUM_RES1
                                             REPLACE VLR_BASE  WITH VLR_ORIGIN
                                          ENDIF
                                          eFAZ2 = .F.
                                       ELSE
                                          * Informaá‰es da Planilha de Correcao
                                          REPLACE INDICE    WITH eINDICE
                                          REPLACE TAXA_MES  WITH eULT_INDI
                                          REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                          REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                          eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                          REPLACE RESI_ACUM WITH eACUM_RES1
                                          REPLACE VLR_BASE  WITH VLR_ORIGIN
                                       ENDIF
                                    ENDIF                &&&
                                    eFAZ = .F.
                                 ELSE
                                    IF ePLANILHA  &&&
                                       IF eRESI_SEPA
                                          IF DATA_VENC > IIF(eMES_LIMIT = "02",CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT),CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT)) .AND. TIPO_PARC <> "R99" && Opcao para fevereiro.
                                             REPLACE VLR_PARC WITH (VLR_ORIGIN * eIND_ACUM)
                                          ENDIF
                                          * Criaá∆o da Parcela de Res°duo
                                          eRESI_SEPA = .F.
                                          eREGISTRO = RECNO()
                                          eCLASSE = CLASSE
                                          eQUADRA = QUADRA
                                          eNUM_IMOVEL = NUM_IMOVEL
                                          ePARTE      = PARTE
                                          eNUM_PARC   = "    0"
                                          eTIPO_PARC  = "R99"
                                          eDATA_VENC  = DATA_VENC
                                          eVLR_PARC2  = eACUM_RES1
                                          eOBS        = "PARC.RESIDUO"
                                          eVLR_ORIGIN = eACUM_RES1
                                          eATIVA      = .T.
                                          eCONTABIL   = .T.
                                          eMULTA      = MULTA
                                          eCOMISSAO   = COMISSAO
                                          ePERID_COMI = "a.m."
                                          GOTO TOP
                                          APPEND BLANK
                                          IF F_BLOQR()
                                             REPLACE CLASSE      WITH eCLASSE,;
                                                     QUADRA      WITH eQUADRA,;
                                                     NUM_IMOVEL  WITH eNUM_IMOVEL
                                             REPLACE PARTE       WITH ePARTE,;
                                                     NUM_PARC    WITH eNUM_PARC,;
                                                     TIPO_PARC   WITH eTIPO_PARC
                                             REPLACE DATA_VENC   WITH eDATA_VENC,;
                                                     VLR_PARC    WITH eVLR_PARC2,;
                                                     OBS         WITH eOBS
                                             REPLACE VLR_ORIGIN  WITH eVLR_ORIGIN,;
                                                     ATIVA       WITH eATIVA,;
                                                     CONTABIL    WITH eCONTABIL
                                             REPLACE MULTA       WITH eMULTA,;
                                                     COMISSAO    WITH eCOMISSAO,;
                                                     PERID_COMI  WITH ePERID_COMI
                                             UNLOCK
                                          ENDIF
                                          GOTO eREGISTRO
                                          F_BLOQR()
                                       ELSE
                                          IF DATA_VENC > IIF(eMES_LIMIT = "02",CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT),CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT)) .AND. TIPO_PARC <> "R99" && Opcao para fevereiro.
                                             REPLACE VLR_PARC WITH (VLR_ORIGIN * eIND_ACUM)
                                          ENDIF
                                       ENDIF
                                       IF eFAZ2
                                          IF SUBSTR(DTOC(DATA_VENC),4,2) = eMES_VENDA
                                             * Informaá‰es da Planilha de Correcao
                                             REPLACE INDICE    WITH eINDICE
                                             REPLACE TAXA_MES  WITH 0
                                             REPLACE TAXA_ACUM WITH 0
                                             REPLACE RESI_MES  WITH 0
                                             REPLACE RESI_ACUM WITH 0
                                             REPLACE VLR_BASE  WITH VLR_ORIGIN
                                          ELSE
                                             * Informaá‰es da Planilha de Correcao
                                             REPLACE INDICE    WITH eINDICE
                                             REPLACE TAXA_MES  WITH eULT_INDI
                                             REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                             REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                             eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                             REPLACE RESI_ACUM WITH eACUM_RES1
                                             REPLACE VLR_BASE  WITH VLR_ORIGIN
                                          ENDIF
                                          eFAZ2 = .F.
                                       ELSE
                                          * Informaá‰es da Planilha de Correcao
                                          REPLACE INDICE    WITH eINDICE
                                          REPLACE TAXA_MES  WITH eULT_INDI
                                          REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                          REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                          eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                          REPLACE RESI_ACUM WITH eACUM_RES1
                                          REPLACE VLR_BASE  WITH VLR_ORIGIN
                                       ENDIF
                                    ELSE
                                       IF eFAZ2
                                          IF SUBSTR(DTOC(DATA_VENC),4,2) = eMES_VENDA
                                             * Informaá‰es da Planilha de Correcao
                                             REPLACE INDICE    WITH eINDICE
                                             REPLACE TAXA_MES  WITH 0
                                             REPLACE TAXA_ACUM WITH 0
                                             REPLACE RESI_MES  WITH 0
                                             REPLACE RESI_ACUM WITH 0
                                             REPLACE VLR_BASE  WITH VLR_ORIGIN
                                          ELSE
                                             * Informaá‰es da Planilha de Correcao
                                             REPLACE INDICE    WITH eINDICE
                                             REPLACE TAXA_MES  WITH eULT_INDI
                                             REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                             REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                             eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                             REPLACE RESI_ACUM WITH eACUM_RES1
                                             REPLACE VLR_BASE  WITH VLR_ORIGIN
                                          ENDIF
                                          eFAZ2 = .F.
                                       ELSE
                                          * Informaá‰es da Planilha de Correcao
                                          REPLACE INDICE    WITH eINDICE
                                          REPLACE TAXA_MES  WITH eULT_INDI
                                          REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                          REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                          eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                          REPLACE RESI_ACUM WITH eACUM_RES1
                                          REPLACE VLR_BASE  WITH VLR_ORIGIN
                                       ENDIF
                                    ENDIF
                                 ENDIF
                              ELSE
                                 IF VLR_PGTO = 0
                                    IF eFAZ
                                       IF ePLANILHA
                                          IF eRESI_SEPA
                                             IF DATA_VENC > IIF(eMES_LIMIT = "02",CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT),CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT)) .AND. TIPO_PARC <> "R99" && Opcao para fevereiro.
                                                REPLACE VLR_PARC WITH (VLR_ORIGIN * eIND_ACUM)
                                             ENDIF
                                             * Criaá∆o da Parcela de Res°duo
                                             eRESI_SEPA = .F.
                                             eREGISTRO = RECNO()
                                             eCLASSE = CLASSE
                                             eQUADRA = QUADRA
                                             eNUM_IMOVEL = NUM_IMOVEL
                                             ePARTE      = PARTE
                                             eNUM_PARC   = "    0"
                                             eTIPO_PARC  = "R99"
                                             eDATA_VENC  = DATA_VENC
                                             eVLR_PARC2  = eACUM_RES1
                                             eOBS        = "PARC.RESIDUO"
                                             eVLR_ORIGIN = eACUM_RES1
                                             eATIVA      = .T.
                                             eCONTABIL   = .T.
                                             eMULTA      = MULTA
                                             eCOMISSAO   = COMISSAO
                                             ePERID_COMI = "a.m."
                                             GOTO TOP
                                             APPEND BLANK
                                             IF F_BLOQR()
                                                REPLACE CLASSE      WITH eCLASSE,;
                                                        QUADRA      WITH eQUADRA,;
                                                        NUM_IMOVEL  WITH eNUM_IMOVEL
                                                REPLACE PARTE       WITH ePARTE,;
                                                        NUM_PARC    WITH eNUM_PARC,;
                                                        TIPO_PARC   WITH eTIPO_PARC
                                                REPLACE DATA_VENC   WITH eDATA_VENC,;
                                                        VLR_PARC    WITH eVLR_PARC2,;
                                                        OBS         WITH eOBS
                                                REPLACE VLR_ORIGIN  WITH eVLR_ORIGIN,;
                                                        ATIVA       WITH eATIVA,;
                                                        CONTABIL    WITH eCONTABIL
                                                REPLACE MULTA       WITH eMULTA,;
                                                        COMISSAO    WITH eCOMISSAO,;
                                                        PERID_COMI  WITH ePERID_COMI
                                                UNLOCK
                                             ENDIF
                                             GOTO eREGISTRO
                                             F_BLOQR()
                                          ELSE
                                             IF DATA_VENC > IIF(eMES_LIMIT = "02",CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT),CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT)) .AND. TIPO_PARC <> "R99" && Opcao para fevereiro.
                                                REPLACE VLR_PARC WITH (VLR_ORIGIN * eIND_ACUM)
                                             ENDIF
                                          ENDIF
                                          IF eFAZ2
                                             IF SUBSTR(DTOC(DATA_VENC),4,2) = eMES_VENDA
                                                * Informaá‰es da Planilha de Correcao
                                                REPLACE INDICE    WITH eINDICE
                                                REPLACE TAXA_MES  WITH 0
                                                REPLACE TAXA_ACUM WITH 0
                                                REPLACE RESI_MES  WITH 0
                                                REPLACE RESI_ACUM WITH 0
                                                REPLACE VLR_BASE  WITH VLR_ORIGIN
                                             ELSE
                                                * Informaá‰es da Planilha de Correcao
                                                REPLACE INDICE    WITH eINDICE
                                                REPLACE TAXA_MES  WITH eULT_INDI
                                                REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                                REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                                eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                                REPLACE RESI_ACUM WITH eACUM_RES1
                                                REPLACE VLR_BASE  WITH VLR_ORIGIN
                                             ENDIF
                                             eFAZ2 = .F.
                                          ELSE
                                             * Informaá‰es da Planilha de Correcao
                                             REPLACE INDICE    WITH eINDICE
                                             REPLACE TAXA_MES  WITH eULT_INDI
                                             REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                             REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                             eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                             REPLACE RESI_ACUM WITH eACUM_RES1
                                             REPLACE VLR_BASE  WITH VLR_ORIGIN
                                          ENDIF
                                       ELSE
                                          IF eFAZ2
                                             IF SUBSTR(DTOC(DATA_VENC),4,2) = eMES_VENDA
                                                * Informaá‰es da Planilha de Correcao
                                                REPLACE INDICE    WITH eINDICE
                                                REPLACE TAXA_MES  WITH 0
                                                REPLACE TAXA_ACUM WITH 0
                                                REPLACE RESI_MES  WITH 0
                                                REPLACE RESI_ACUM WITH 0
                                                REPLACE VLR_BASE  WITH VLR_ORIGIN
                                             ELSE
                                                * Informaá‰es da Planilha de Correcao
                                                REPLACE INDICE    WITH eINDICE
                                                REPLACE TAXA_MES  WITH eULT_INDI
                                                REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                                REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                                eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                                REPLACE RESI_ACUM WITH eACUM_RES1
                                                REPLACE VLR_BASE  WITH VLR_ORIGIN
                                             ENDIF
                                             eFAZ2 = .F.
                                          ELSE
                                             * Informaá‰es da Planilha de Correcao
                                             REPLACE INDICE    WITH eINDICE
                                             REPLACE TAXA_MES  WITH eULT_INDI
                                             REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                             REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                             eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                             REPLACE RESI_ACUM WITH eACUM_RES1
                                             REPLACE VLR_BASE  WITH VLR_ORIGIN
                                          ENDIF
                                       ENDIF
                                       eFAZ = .F.
                                    ELSE
                                       IF ePLANILHA
                                          IF eRESI_SEPA
                                             IF DATA_VENC > IIF(eMES_LIMIT = "02",CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT),CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT)) .AND. TIPO_PARC <> "R99" && Opcao para fevereiro.
                                                REPLACE VLR_PARC WITH (VLR_ORIGIN * eIND_ACUM)
                                             ENDIF
                                             * Criaá∆o da Parcela de Res°duo
                                             eRESI_SEPA = .F.
                                             eREGISTRO = RECNO()
                                             eCLASSE = CLASSE
                                             eQUADRA = QUADRA
                                             eNUM_IMOVEL = NUM_IMOVEL
                                             ePARTE      = PARTE
                                             eNUM_PARC   = "    0"
                                             eTIPO_PARC  = "R99"
                                             eDATA_VENC  = DATA_VENC
                                             eVLR_PARC2  = eACUM_RES1
                                             eOBS        = "PARC.RESIDUO"
                                             eVLR_ORIGIN = eACUM_RES1
                                             eATIVA      = .T.
                                             eCONTABIL   = .T.
                                             eMULTA      = MULTA
                                             eCOMISSAO   = COMISSAO
                                             ePERID_COMI = "a.m."
                                             APPEND BLANK
                                             IF F_BLOQR()
                                                REPLACE CLASSE      WITH eCLASSE,;
                                                        QUADRA      WITH eQUADRA,;
                                                        NUM_IMOVEL  WITH eNUM_IMOVEL
                                                REPLACE PARTE       WITH ePARTE,;
                                                        NUM_PARC    WITH eNUM_PARC,;
                                                        TIPO_PARC   WITH eTIPO_PARC
                                                REPLACE DATA_VENC   WITH eDATA_VENC,;
                                                        VLR_PARC    WITH eVLR_PARC2,;
                                                        OBS         WITH eOBS
                                                REPLACE VLR_ORIGIN  WITH eVLR_ORIGIN,;
                                                        ATIVA       WITH eATIVA,;
                                                        CONTABIL    WITH eCONTABIL
                                                REPLACE MULTA       WITH eMULTA,;
                                                        COMISSAO    WITH eCOMISSAO,;
                                                        PERID_COMI  WITH ePERID_COMI
                                                UNLOCK
                                             ENDIF
                                             GOTO eREGISTRO
                                             F_BLOQR()
                                          ELSE
                                             IF DATA_VENC > IIF(eMES_LIMIT = "02",CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT),CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT)) .AND. TIPO_PARC <> "R99" && Opcao para fevereiro.
                                                REPLACE VLR_PARC WITH (VLR_ORIGIN * eIND_ACUM)
                                             ENDIF
                                          ENDIF
                                          IF eFAZ2
                                             IF SUBSTR(DTOC(DATA_VENC),4,2) = eMES_VENDA
                                                * Informaá‰es da Planilha de Correcao
                                                REPLACE INDICE    WITH eINDICE
                                                REPLACE TAXA_MES  WITH 0
                                                REPLACE TAXA_ACUM WITH 0
                                                REPLACE RESI_MES  WITH 0
                                                REPLACE RESI_ACUM WITH 0
                                                REPLACE VLR_BASE  WITH VLR_ORIGIN
                                             ELSE
                                                * Informaá‰es da Planilha de Correcao
                                                REPLACE INDICE    WITH eINDICE
                                                REPLACE TAXA_MES  WITH eULT_INDI
                                                REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                                REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                                eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                                REPLACE RESI_ACUM WITH eACUM_RES1
                                                REPLACE VLR_BASE  WITH VLR_ORIGIN
                                             ENDIF
                                             eFAZ2 = .F.
                                          ELSE
                                             * Informaá‰es da Planilha de Correcao
                                             REPLACE INDICE    WITH eINDICE
                                             REPLACE TAXA_MES  WITH eULT_INDI
                                             REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                             REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                             eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                             REPLACE RESI_ACUM WITH eACUM_RES1
                                             REPLACE VLR_BASE  WITH VLR_ORIGIN
                                          ENDIF
                                       ELSE
                                          IF eFAZ2
                                             IF SUBSTR(DTOC(DATA_VENC),4,2) = eMES_VENDA
                                                * Informaá‰es da Planilha de Correcao
                                                REPLACE INDICE    WITH eINDICE
                                                REPLACE TAXA_MES  WITH 0
                                                REPLACE TAXA_ACUM WITH 0
                                                REPLACE RESI_MES  WITH 0
                                                REPLACE RESI_ACUM WITH 0
                                                REPLACE VLR_BASE  WITH VLR_ORIGIN
                                             ELSE
                                                * Informaá‰es da Planilha de Correcao
                                                REPLACE INDICE    WITH eINDICE
                                                REPLACE TAXA_MES  WITH eULT_INDI
                                                REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                                REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                                eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                                REPLACE RESI_ACUM WITH eACUM_RES1
                                                REPLACE VLR_BASE  WITH VLR_ORIGIN
                                             ENDIF
                                             eFAZ2 = .F.
                                          ELSE
                                             * Informaá‰es da Planilha de Correcao
                                             REPLACE INDICE    WITH eINDICE
                                             REPLACE TAXA_MES  WITH eULT_INDI
                                             REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                             REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                             eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                             REPLACE RESI_ACUM WITH eACUM_RES1
                                             REPLACE VLR_BASE  WITH VLR_ORIGIN
                                          ENDIF
                                       ENDIF
                                    ENDIF
                                 ENDIF
                              ENDIF
                           ENDIF
                        ELSE
                           IF DATA_VENC > IIF(eMES_VENDA = "2",CTOD("28/"+eMES_VENDA+"/"+eANO_VENDA),CTOD("30/"+eMES_VENDA+"/"+eANO_VENDA)) .AND. DATA_VENC < IIF(eMES_LIMIT = "2",CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT),CTOD("30/"+eMES_LIMIT+"/"+eANO_LIMIT))
                              IF eFAZ2
                                 IF SUBSTR(DTOC(DATA_VENC),4,2) = eMES_VENDA
                                    * Informaá‰es da Planilha de Correcao
                                    REPLACE INDICE    WITH eINDICE
                                    REPLACE TAXA_MES  WITH 0
                                    REPLACE TAXA_ACUM WITH 0
                                    REPLACE RESI_MES  WITH 0
                                    REPLACE RESI_ACUM WITH 0
                                    REPLACE VLR_BASE  WITH VLR_ORIGIN
                                 ELSE
                                    * Informaá‰es da Planilha de Correcao
                                    REPLACE INDICE    WITH eINDICE
                                    REPLACE TAXA_MES  WITH eULT_INDI
                                    REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                    REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                    eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                    REPLACE RESI_ACUM WITH eACUM_RES1
                                    REPLACE VLR_BASE  WITH VLR_ORIGIN
                                 ENDIF
                                 eFAZ2 = .F.
                              ELSE
                                 * Informaá‰es da Planilha de Correcao
                                 REPLACE INDICE    WITH eINDICE
                                 REPLACE TAXA_MES  WITH eULT_INDI
                                 REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                 REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                 eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                 REPLACE RESI_ACUM WITH eACUM_RES1
                                 REPLACE VLR_BASE  WITH VLR_ORIGIN
                              ENDIF
                           ENDIF
                        ENDIF
*********************
                     ELSE
********************* SEM RESIDUO INTEGRAL NA 13a.
                        IF DATA_VENC > IIF(eMES_LIMIT = "02",CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT),CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT))  && Opcao para fevereiro.
                           IF eCORR_BOLE
                              IF eCORR_PGTO
                                 IF ePLANILHA
                                    IF eRESI_SEPA
                                       IF DATA_VENC > IIF(eMES_LIMIT = "02",CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT),CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT)) .AND. TIPO_PARC <> "R99"  && Opcao para fevereiro. 
                                          REPLACE VLR_PARC WITH (VLR_ORIGIN * eIND_ACUM)
                                       ENDIF
                                       * Criaá∆o da Parcela de Res°duo
                                       eRESI_SEPA = .F.
                                       eREGISTRO = RECNO()
                                       eCLASSE = CLASSE
                                       eQUADRA = QUADRA
                                       eNUM_IMOVEL = NUM_IMOVEL
                                       ePARTE      = PARTE
                                       eNUM_PARC   = "    0"
                                       eTIPO_PARC  = "R99"
                                       eDATA_VENC  = DATA_VENC
                                       eVLR_PARC2  = eACUM_RES1
                                       eOBS        = "PARC.RESIDUO"
                                       eVLR_ORIGIN = eACUM_RES1
                                       eATIVA      = .T.
                                       eCONTABIL   = .T.
                                       eMULTA      = MULTA
                                       eCOMISSAO   = COMISSAO
                                       ePERID_COMI = "a.m."
                                       GOTO TOP
                                       APPEND BLANK
                                       IF F_BLOQR()
                                          REPLACE CLASSE      WITH eCLASSE,;
                                                  QUADRA      WITH eQUADRA,;
                                                  NUM_IMOVEL  WITH eNUM_IMOVEL
                                          REPLACE PARTE       WITH ePARTE,;
                                                  NUM_PARC    WITH eNUM_PARC,;
                                                  TIPO_PARC   WITH eTIPO_PARC
                                          REPLACE DATA_VENC   WITH eDATA_VENC,;
                                                  VLR_PARC    WITH eVLR_PARC2,;
                                                  OBS         WITH eOBS
                                          REPLACE VLR_ORIGIN  WITH eVLR_ORIGIN,;
                                                  ATIVA       WITH eATIVA,;
                                                  CONTABIL    WITH eCONTABIL
                                          REPLACE MULTA       WITH eMULTA,;
                                                  COMISSAO    WITH eCOMISSAO,;
                                                  PERID_COMI  WITH ePERID_COMI
                                          UNLOCK
                                       ENDIF
                                       GOTO eREGISTRO
                                       F_BLOQR()
                                    ELSE

                                       IF DATA_VENC > IIF(eMES_LIMIT = "02",CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT),CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT)) .AND. TIPO_PARC <> "R99"  && Opcao para fevereiro.
***

                                          REPLACE VLR_PARC WITH (VLR_ORIGIN * eIND_ACUM) + (eACUM_RES1 / eQUANT_PAR)

                                       ENDIF
                                    ENDIF
                                    IF eFAZ2
                                       IF SUBSTR(DTOC(DATA_VENC),4,2) = eMES_VENDA
                                          * Informaá‰es da Planilha de Correcao
                                          REPLACE INDICE    WITH eINDICE
                                          REPLACE TAXA_MES  WITH 0
                                          REPLACE TAXA_ACUM WITH 0
                                          REPLACE RESI_MES  WITH 0
                                          REPLACE RESI_ACUM WITH 0
                                          REPLACE VLR_BASE  WITH VLR_ORIGIN
                                       ELSE
                                          * Informaá‰es da Planilha de Correcao
                                          REPLACE INDICE    WITH eINDICE
                                          REPLACE TAXA_MES  WITH eULT_INDI
                                          REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                          REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                          eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                          REPLACE RESI_ACUM WITH eACUM_RES1
                                          REPLACE VLR_BASE  WITH VLR_ORIGIN
                                       ENDIF
                                       eFAZ2 = .F.
                                    ELSE
                                       * Informaá‰es da Planilha de Correcao
                                       REPLACE INDICE    WITH eINDICE
                                       REPLACE TAXA_MES  WITH eULT_INDI
                                       REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                       REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                       eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                       REPLACE RESI_ACUM WITH eACUM_RES1
                                       REPLACE VLR_BASE  WITH VLR_ORIGIN
                                    ENDIF
                                 ELSE
                                    IF eFAZ2
                                       IF SUBSTR(DTOC(DATA_VENC),4,2) = eMES_VENDA
                                          * Informaá‰es da Planilha de Correcao
                                          REPLACE INDICE    WITH eINDICE
                                          REPLACE TAXA_MES  WITH 0
                                          REPLACE TAXA_ACUM WITH 0
                                          REPLACE RESI_MES  WITH 0
                                          REPLACE RESI_ACUM WITH 0
                                          REPLACE VLR_BASE  WITH VLR_ORIGIN
                                       ELSE
                                          * Informaá‰es da Planilha de Correcao
                                          REPLACE INDICE    WITH eINDICE
                                          REPLACE TAXA_MES  WITH eULT_INDI
                                          REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                          REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                          eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                          REPLACE RESI_ACUM WITH eACUM_RES1
                                          REPLACE VLR_BASE  WITH VLR_ORIGIN
                                       ENDIF
                                       eFAZ2 = .F.
                                    ELSE
                                       * Informaá‰es da Planilha de Correcao
                                       REPLACE INDICE    WITH eINDICE
                                       REPLACE TAXA_MES  WITH eULT_INDI
                                       REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                       REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                       eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                       REPLACE RESI_ACUM WITH eACUM_RES1
                                       REPLACE VLR_BASE  WITH VLR_ORIGIN
                                    ENDIF
                                 ENDIF
                              ELSE
                                 IF VLR_PGTO = 0
                                    IF ePLANILHA
                                       IF eRESI_SEPA
                                          IF DATA_VENC > IIF(eMES_LIMIT = "02",CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT),CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT)) .AND. TIPO_PARC <> "R99"  && Opcao para fevereiro. 
                                             REPLACE VLR_PARC WITH (VLR_ORIGIN * eIND_ACUM)
                                          ENDIF
                                          * Criaá∆o da Parcela de Res°duo
                                          eRESI_SEPA = .F.
                                          eREGISTRO = RECNO()
                                          eCLASSE = CLASSE
                                          eQUADRA = QUADRA
                                          eNUM_IMOVEL = NUM_IMOVEL
                                          ePARTE      = PARTE
                                          eNUM_PARC   = "    0"
                                          eTIPO_PARC  = "R99"
                                          eDATA_VENC  = DATA_VENC
                                          eVLR_PARC2  = eACUM_RES1
                                          eOBS        = "PARC.RESIDUO"
                                          eVLR_ORIGIN = eACUM_RES1
                                          eATIVA      = .T.
                                          eCONTABIL   = .T.
                                          eMULTA      = MULTA
                                          eCOMISSAO   = COMISSAO
                                          ePERID_COMI = "a.m."
                                          GOTO TOP
                                          APPEND BLANK
                                          IF F_BLOQR()
                                             REPLACE CLASSE      WITH eCLASSE,;
                                                     QUADRA      WITH eQUADRA,;
                                                     NUM_IMOVEL  WITH eNUM_IMOVEL
                                             REPLACE PARTE       WITH ePARTE,;
                                                     NUM_PARC    WITH eNUM_PARC,;
                                                     TIPO_PARC   WITH eTIPO_PARC
                                             REPLACE DATA_VENC   WITH eDATA_VENC,;
                                                     VLR_PARC    WITH eVLR_PARC,;
                                                     OBS         WITH eOBS
                                             REPLACE VLR_ORIGIN  WITH eVLR_ORIGIN,;
                                                     ATIVA       WITH eATIVA,;
                                                     CONTABIL    WITH eCONTABIL
                                             REPLACE MULTA       WITH eMULTA,;
                                                     COMISSAO    WITH eCOMISSAO,;
                                                     PERID_COMI  WITH ePERID_COMI
                                             UNLOCK
                                          ENDIF
                                          GOTO eREGISTRO
                                          F_BLOQR()
                                       ELSE
                                          IF DATA_VENC > IIF(eMES_LIMIT = "02",CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT),CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT)) .AND. TIPO_PARC <> "R99"  && Opcao para fevereiro. 

                                             REPLACE VLR_PARC WITH (VLR_ORIGIN * eIND_ACUM)

                                          ENDIF
                                       ENDIF
                                       IF eFAZ2
                                          IF SUBSTR(DTOC(DATA_VENC),4,2) = eMES_VENDA
                                             * Informaá‰es da Planilha de Correcao
                                             REPLACE INDICE    WITH eINDICE
                                             REPLACE TAXA_MES  WITH 0
                                             REPLACE TAXA_ACUM WITH 0
                                             REPLACE RESI_MES  WITH 0
                                             REPLACE RESI_ACUM WITH 0
                                             REPLACE VLR_BASE  WITH VLR_ORIGIN
                                          ELSE
                                             * Informaá‰es da Planilha de Correcao
                                             REPLACE INDICE    WITH eINDICE
                                             REPLACE TAXA_MES  WITH eULT_INDI
                                             REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                             REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                             eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                             REPLACE RESI_ACUM WITH eACUM_RES1
                                             REPLACE VLR_BASE  WITH VLR_ORIGIN
                                          ENDIF
                                          eFAZ2 = .F.
                                       ELSE
                                          * Informaá‰es da Planilha de Correcao
                                          REPLACE INDICE    WITH eINDICE
                                          REPLACE TAXA_MES  WITH eULT_INDI
                                          REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                          REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                          eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                          REPLACE RESI_ACUM WITH eACUM_RES1
                                          REPLACE VLR_BASE  WITH VLR_ORIGIN
                                       ENDIF
                                    ELSE
                                       IF eFAZ2
                                          IF SUBSTR(DTOC(DATA_VENC),4,2) = eMES_VENDA
                                             * Informaá‰es da Planilha de Correcao
                                             REPLACE INDICE    WITH eINDICE
                                             REPLACE TAXA_MES  WITH 0
                                             REPLACE TAXA_ACUM WITH 0
                                             REPLACE RESI_MES  WITH 0
                                             REPLACE RESI_ACUM WITH 0
                                             REPLACE VLR_BASE  WITH VLR_ORIGIN
                                          ELSE
                                             * Informaá‰es da Planilha de Correcao
                                             REPLACE INDICE    WITH eINDICE
                                             REPLACE TAXA_MES  WITH eULT_INDI
                                             REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                             REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                             eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                             REPLACE RESI_ACUM WITH eACUM_RES1
                                             REPLACE VLR_BASE  WITH VLR_ORIGIN
                                          ENDIF
                                          eFAZ2 = .F.
                                       ELSE
                                          * Informaá‰es da Planilha de Correcao
                                          REPLACE INDICE    WITH eINDICE
                                          REPLACE TAXA_MES  WITH eULT_INDI
                                          REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                          REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                          eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                          REPLACE RESI_ACUM WITH eACUM_RES1
                                          REPLACE VLR_BASE  WITH VLR_ORIGIN
                                       ENDIF
                                    ENDIF
                                 ENDIF
                              ENDIF
                           ENDIF
                        ELSE
                           IF DATA_VENC > CTOD("01/"+eMES_VENDA+"/"+eANO_VENDA) .AND. DATA_VENC < IIF(eMES_LIMIT="2",CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT),CTOD("30/"+eMES_LIMIT+"/"+eANO_LIMIT))
                              IF eFAZ2
                                 IF SUBSTR(DTOC(DATA_VENC),4,2) = eMES_VENDA
                                    * Informaá‰es da Planilha de Correcao
                                    REPLACE INDICE    WITH eINDICE
                                    REPLACE TAXA_MES  WITH 0
                                    REPLACE TAXA_ACUM WITH 0
                                    REPLACE RESI_MES  WITH 0
                                    REPLACE RESI_ACUM WITH 0
                                    REPLACE VLR_BASE  WITH VLR_ORIGIN
                                 ELSE
                                    * Informaá‰es da Planilha de Correcao
                                    REPLACE INDICE    WITH eINDICE
                                    REPLACE TAXA_MES  WITH eULT_INDI
                                    REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                    REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                    eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                    REPLACE RESI_ACUM WITH eACUM_RES1
                                    REPLACE VLR_BASE  WITH VLR_ORIGIN
                                 ENDIF
                                 eFAZ2 = .F.
                              ELSE
                                 * Informaá‰es da Planilha de Correcao
                                 REPLACE INDICE    WITH eINDICE
                                 REPLACE TAXA_MES  WITH eULT_INDI
                                 REPLACE TAXA_ACUM WITH ((((eIND_ACUM*(1/((eULT_IND2/100)+1)))) - 1) * 100)
                                 REPLACE RESI_MES  WITH eACUM_RES1 - eACUM_RESA
                                 eACUM_RES1 = ((eACUM_RES1 - (eACUM_RES1 - eACUM_RESA)) * ((eULT_INDI/100)+1)) + (eACUM_RES1 - eACUM_RESA)
                                 REPLACE RESI_ACUM WITH eACUM_RES1
                                 REPLACE VLR_BASE  WITH VLR_ORIGIN
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
                     UNLOCK
                  ENDIF
                  SELE 8
                  SKIP
               ENDDO
               SELE 6
               GOTO TOP
               SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
               IF F_BLOQR()
                  REPLACE ULT_CORREC WITH (CTOD("01/"+eMES_LIMIT+"/"+eANO_LIMIT)+32)
                  UNLOCK
               ENDIF
               SKIP
            ELSE
               SELE 6
               SKIP
            ENDIF
RETURN nil
*************************  fim  *********************************
function COR_ANUAL12()
            * Correcao Anual de Contratos 12 Meses
            PLQUA("01")
            PMENSAG("Anual 12 Meses ...")
            SELE 6
            IF ULT_CORREC < CTOD("01/"+SUBSTR(DTOC(eDATA_FIM),4,2)+"/"+STR((VAL(SUBSTR(DTOC(eDATA_FIM),7,4))-1),4))
               SELE 8
               GOTO TOP
               PLQUA("01")
	       PMENSAG("Preparando Parcelas ...")
               eQUANT_PAR = 1
               DO WHILE .NOT. EOF()
                  SELE 8
                  IF DATA_VENC >= CTOD("01/"+SUBSTR(DTOC(eDATA_FIM),4,2)+"/"+SUBSTR(DTOC(eDATA_FIM),7,4)) .AND. VLR_PGTO = 0
                     eQUANT_PAR = eQUANT_PAR + 1
                  ENDIF
                  SKIP
               ENDDO
               SELE 8
               GOTO TOP
               eACUM_RES1 = 0
               eACUM_TAXA = 1
               eACUM_RESA = 0
               DO WHILE .NOT. EOF()
                  PLQUA("01")
		  PMENSAG("Efetuando Correá∆o das Parcelas do Im¢vel ("+ALLTRIM(STR(eCLAS_PRO,5))+"/"+ALLTRIM(eQUADRA)+"/"+ALLTRIM(eNUM_IMOVEL)+"/"+STR(ePARTE,1)+") Parcela No. "+NUM_PARC)
                  eMES_LIMIT = STR(IIF((VAL(SUBSTR(DTOC(eDATA_FIM),4,2))-2) <= 0,IIF((VAL(SUBSTR(DTOC(eDATA_FIM),4,2))-2) = 0,12,11),(VAL(SUBSTR(DTOC(eDATA_FIM),4,2))-2)),2)
                  eMES_LIMIT = IIF(LEN(ALLTRIM(eMES_LIMIT)) = 1,"0"+ALLTRIM(eMES_LIMIT),ALLTRIM(eMES_LIMIT))
                  eANO_LIMIT = STR(IIF((VAL(SUBSTR(DTOC(eDATA_FIM),4,2))-2) <= 0,VAL(SUBSTR(DTOC(eDATA_FIM),7,4))-1,VAL(SUBSTR(DTOC(eDATA_FIM),7,4))),4)
                  eANO_FIMCR = STR((VAL(SUBSTR(DTOC(eDATA_FIM),7,4))+1),4)
                  eMES_FIMCR = STR(IIF((VAL(SUBSTR(DTOC(eDATA_FIM),4,2))-2) <= 0,IIF((VAL(SUBSTR(DTOC(eDATA_FIM),4,2))-2) = 0,12,11),(VAL(SUBSTR(DTOC(eDATA_FIM),4,2))-2)),2)
                  eMES_FIMCR = IIF(LEN(ALLTRIM(eMES_FIMCR)) = 1,"0"+ALLTRIM(eMES_FIMCR),ALLTRIM(eMES_FIMCR))
                  eVLR_ORIG  = VLR_PARC
                  eDATA_VENC = DATA_VENC
                  eCOD_BANCO = COD_BANCO
                  eMES_PARCE = SUBSTR(DTOC(eDATA_VENC),4,2)
                  eANO_PARCE = SUBSTR(DTOC(eDATA_VENC),7,4)
                  eMES_VENDA = STR(IIF((VAL(SUBSTR(DTOC(eDATA_FIM),4,2))-1) <= 0,12,(VAL(SUBSTR(DTOC(eDATA_FIM),4,2))-1)),2)
                  eMES_VENDA = IIF(LEN(ALLTRIM(eMES_VENDA)) = 1,"0"+ALLTRIM(eMES_VENDA),ALLTRIM(eMES_VENDA))
                  eANO_VENDA = IIF((VAL(SUBSTR(DTOC(eDATA_FIM),4,2))-1) <= 0,ALLTRIM(STR((VAL(SUBSTR(DTOC(eDATA_FIM),7,4))-2),4)),ALLTRIM(STR((VAL(SUBSTR(DTOC(eDATA_FIM),7,4))-1),4)))
                  SELE 7
                  SET FILTER TO ((ANO+MES >= eANO_VENDA+eMES_VENDA) .AND. (ANO+MES <= eANO_LIMIT+eMES_LIMIT))
                  GOTO TOP
                  SELE 7         
                  GOTO TOP
                  eIND_ACUM  = 1
                  * Acumular o Indice de 12 Meses
                  DO WHILE (ANO+MES <= eANO_PARCE+eMES_PARCE) .AND. .NOT. EOF()
                     eIND_ACUM =  eIND_ACUM * ((&eINDICE/100) + 1)
                     SKIP
                  ENDDO
                  SELE 8
                  @ 12,00 SAY "Valor Original : " GET VLR_ORIGIN PICT "@R 999,999,999.99"
                  @ 13,00 SAY "Valor Parcela  : " GET VLR_PARC   PICT "@R 999,999,999.99"
                  @ 14,00 SAY "Indice Acum.   : " GET eIND_ACUM  PICT "@R   999,999.9999"
                  @ 15,00 SAY "Residuo Acum.  : " GET eACUM_RES1 PICT "@R 999,999,999.99"
                  eULT_INDI  = 0
                  eULT_IND2  = 0
                  * Acumular o Indice do residuo
                  IF DATA_VENC >= CTOD("01/"+eMES_VENDA+"/"+eANO_VENDA) .AND. DATA_VENC <= CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT)
                     SELE 7
                     SET FILTER TO ((ANO+MES >= eANO_VENDA+eMES_VENDA) .AND. (ANO+MES <= eANO_LIMIT+eMES_LIMIT))
                     GOTO TOP
                     eIND_RESI  = 1
                     eULT_INDI  = 0
                     eULT_IND2  = 0
                     DO WHILE (ANO+MES < eANO_PARCE+eMES_PARCE) .AND. .NOT. EOF()
                        eIND_RESI = eIND_RESI * ((&eINDICE/100) + 1)
                        SKIP 
                     ENDDO
                     eULT_IND2 = &eINDICE
                     SKIP - 1
                     eULT_INDI = &eINDICE
                     SELE 8
                     eACUM_RESA = eACUM_RES1
                     IF DATA_VENC > IIF(eMES_VENDA = "02",CTOD("28/"+eMES_VENDA+"/"+eANO_VENDA),CTOD("30/"+eMES_VENDA+"/"+eANO_VENDA)) && Opcao para fevereiro.
                           eACUM_RES1 = 0
                           @ 15,00 SAY "Residuo Acum.  : " GET eACUM_RES1 PICT "@R 999,999,999.99"
                     ELSE
                           eACUM_RES1 = 0
                           @ 15,00 SAY "Residuo Acum.  : " GET eACUM_RES1 PICT "@R 999,999,999.99"
                     ENDIF
                  ENDIF
                  SELE 8
                  @ 12,00 SAY "Valor Original : " GET VLR_ORIGIN PICT "@R 999,999,999.99"
                  @ 13,00 SAY "Valor Parcela  : " GET VLR_PARC   PICT "@R 999,999,999.99"
                  @ 14,00 SAY "Indice Acum.   : " GET eIND_ACUM  PICT "@R   999,999.9999"
                  @ 15,00 SAY "Residuo Acum.  : " GET eACUM_RES1 PICT "@R 999,999,999.99"

                  eVLR_ATUAL = (VLR_ORIGIN * eIND_ACUM)

                  @ 17,00 SAY "Valor Corrigido: " GET eVLR_ATUAL PICT "@R 999,999,999.99"
                  SELE 8
                  IF F_BLOQR()
                        IF DATA_VENC > CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT) && Opcao para fevereiro.
                           IF eCORR_BOLE
                              IF eCORR_PGTO
                                 * Limitador de 12 Meses
                                 IF DATA_VENC > CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT) .AND. TIPO_PARC <> "R99" .AND. DATA_VENC <= CTOD("28/"+eMES_FIMCR+"/"+eANO_FIMCR)
                                    REPLACE VLR_PARC WITH (VLR_ORIGIN * eIND_ACUM)
                                 ENDIF
                              ELSE
                                 IF VLR_PGTO = 0
                                    * Limitador de 12 Meses
                                    IF DATA_VENC > CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT) .AND. TIPO_PARC <> "R99" .AND. DATA_VENC <= CTOD("28/"+eMES_FIMCR+"/"+eANO_FIMCR)
                                       REPLACE VLR_PARC WITH (VLR_ORIGIN * eIND_ACUM)
                                    ENDIF
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
                     UNLOCK
                  ENDIF
                  SELE 8
                  SKIP
               ENDDO
               SELE 6
               GOTO TOP
               SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
               IF F_BLOQR()
                  REPLACE ULT_CORREC WITH (CTOD("01/"+eMES_LIMIT+"/"+eANO_LIMIT)+32)
                  UNLOCK
               ENDIF
               SKIP
            ELSE
               SELE 6
               SKIP
            ENDIF

RETURN nil
*************************  fim  *********************************
function COR_SEMESTRAL()
            * Correcao Semestral de Contratos
            PLQUA("01")
            PMENSAG("Semestral ...")
            SELE 6
            IF ULT_CORREC < (eDATA_FIM - 182)
               SELE 8
               GOTO TOP
               PLQUA("01")
	       PMENSAG("Preparando Parcelas ...")
               eQUANT_PAR = 0
               DO WHILE .NOT. EOF()
                  SELE 8
                  IF DATA_VENC >= CTOD("01/"+SUBSTR(DTOC(eDATA_FIM),4,2)+"/"+SUBSTR(DTOC(eDATA_FIM),7,4)) .AND. VLR_PGTO = 0
                     eQUANT_PAR = eQUANT_PAR + 1
                  ENDIF
                  IF F_BLOQR()
                     REPLACE VLR_ORIGIN WITH VLR_PARC
                     UNLOCK
                  ENDIF
                  SKIP
               ENDDO
               SELE 8
               GOTO TOP
               eACUM_RES1 = 0
               eACUM_TAXA = 1
               DO WHILE .NOT. EOF()
                  PLQUA("01")
		  PMENSAG("Efetuando Correá∆o das Parcelas do Im¢vel ("+ALLTRIM(STR(eCLAS_PRO,5))+"/"+ALLTRIM(eQUADRA)+"/"+ALLTRIM(eNUM_IMOVEL)+"/"+STR(ePARTE,1)+") Parcela No. "+NUM_PARC)
                  eDATA_AUX = eDATA_FIM - 182
                  eMES_LIMIT = STR(IIF((VAL(SUBSTR(DTOC(eDATA_FIM),4,2))-1) <= 0,12,(VAL(SUBSTR(DTOC(eDATA_FIM),4,2))-1)),2)
                  eANO_LIMIT = STR(IIF((VAL(SUBSTR(DTOC(eDATA_FIM),4,2))-1) <= 0,VAL(SUBSTR(DTOC(eDATA_FIM),7,4))-1,VAL(SUBSTR(DTOC(eDATA_FIM),7,4))),4)
                  eMES_LIMIT = IIF(LEN(eMES_LIMIT) = 1,"0"+ALLTRIM(eMES_LIMIT),ALLTRIM(eMES_LIMIT))
                  eVLR_ORIG  = VLR_PARC
                  eDATA_VENC = DATA_VENC
                  eCOD_BANCO = COD_BANCO
                  eMES_PARCE = SUBSTR(DTOC(eDATA_VENC),4,2)
                  eANO_PARCE = SUBSTR(DTOC(eDATA_VENC),7,4)
                  eMES_VENDA = SUBSTR(DTOC(eDATA_FIM - 182),4,2)
                  eANO_VENDA = ALLTRIM(STR((VAL(SUBSTR(DTOC(eDATA_FIM - 182),7,4))),4))
                  SELE 7         
                  SET FILTER TO ((ANO+MES >= eANO_VENDA+eMES_VENDA) .AND. (ANO+MES <= eANO_LIMIT+eMES_LIMIT))
                  GOTO TOP
                  SELE 7         
                  GOTO TOP
                  eIND_ACUM  = 1
                  eULT_INDI  = 0
                  DO WHILE (ANO+MES <= eANO_PARCE+eMES_PARCE) .AND. .NOT. EOF()
                     eIND_ACUM = ((&eINDICE/100) + 1) * eIND_ACUM
                     SKIP
                  ENDDO
                  SKIP -1
                  eULT_INDI = &eINDICE
                  SELE 8
                  @ 12,00 SAY "Valor Original : " GET VLR_ORIGIN PICT "@R 999,999,999.99"
                  @ 13,00 SAY "Valor Parcela  : " GET VLR_PARC   PICT "@R 999,999,999.99"
                  @ 14,00 SAY "Indice Acum.   : " GET eIND_ACUM  PICT "@R   999,999.9999"
                  @ 15,00 SAY "Residuo Acum.  : " GET eACUM_RES1 PICT "@R 999,999,999.99"
                  IF F_BLOQR()
                     REPLACE INDICE    WITH eINDICE
                     REPLACE TAXA_MES  WITH eULT_INDI
                     REPLACE TAXA_ACUM WITH ((eIND_ACUM - 1) * 100)
                     REPLACE RESI_MES  WITH ((VLR_ORIGIN * eIND_ACUM) - VLR_ORIGIN)
                     REPLACE RESI_ACUM WITH eACUM_RES1
                     REPLACE VLR_BASE  WITH VLR_ORIGIN
                     UNLOCK
                  ENDIF
                  IF DATA_VENC >= CTOD("01/"+eMES_VENDA+"/"+eANO_VENDA) .AND. DATA_VENC <= CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT)
                     SELE 7         
                     SET FILTER TO ((ANO+MES >= eANO_PARCE+eMES_PARCE) .AND. (ANO+MES <= eANO_LIMIT+eMES_LIMIT)) 
                     GOTO TOP
                     eIND_RESI  = 1
                     DO WHILE (ANO+MES <= eANO_LIMIT+eMES_LIMIT) .AND. .NOT. EOF()
                        eIND_RESI = ((&eINDICE/100) + 1) * eIND_RESI
                        SKIP
                     ENDDO
                     SELE 8
                     eACUM_RES1 = eACUM_RES1 + ((((VLR_ORIGIN * eIND_ACUM) - VLR_ORIGIN) / eQUANT_PAR) * eIND_RESI)
                     @ 15,00 SAY "Residuo Acum.  : " GET eACUM_RES1 PICT "@R 999,999,999.99"
                  ENDIF
                  SELE 8
                  @ 12,00 SAY "Valor Original : " GET VLR_ORIGIN PICT "@R 999,999,999.99"
                  @ 13,00 SAY "Valor Parcela  : " GET VLR_PARC   PICT "@R 999,999,999.99"
                  @ 14,00 SAY "Indice Acum.   : " GET eIND_ACUM  PICT "@R   999,999.9999"
                  @ 15,00 SAY "Residuo Acum.  : " GET eACUM_RES1 PICT "@R 999,999,999.99"
                  eVLR_ATUAL = (VLR_ORIGIN * eIND_ACUM) + eACUM_RES1
                  @ 17,00 SAY "Valor Corrigido: " GET eVLR_ATUAL PICT "@R 999,999,999.99"
                  IF F_BLOQR()                 
                     IF DATA_VENC > CTOD("28/"+eMES_LIMIT+"/"+eANO_LIMIT)
                        * ALTERACAO PARA OPERA SEM INCLUIR O RESIDUO DILUIDO
                        IF eCNPJ = "04882865000180"  && OPERA CONSTRUTORA
                           IF eCORR_BOLE
                              IF eCORR_PGTO
                                 IF ePLANILHA &&&
                                    REPLACE VLR_PARC WITH (VLR_ORIGIN * eIND_ACUM)
                                 ENDIF
                              ELSE
                                 IF VLR_PGTO = 0
                                    IF ePLANILHA
                                       REPLACE VLR_PARC WITH (VLR_ORIGIN * eIND_ACUM)
                                    ENDIF
                                 ENDIF
                              ENDIF
                           ENDIF
                        ELSE
                           IF eCORR_BOLE
                              IF eCORR_PGTO
                                 IF ePLANILHA &&&
                                    REPLACE VLR_PARC WITH (VLR_ORIGIN * eIND_ACUM) + eACUM_RES1
                                 ENDIF
                              ELSE
                                 IF VLR_PGTO = 0
                                    IF ePLANILHA
                                       REPLACE VLR_PARC WITH (VLR_ORIGIN * eIND_ACUM) + eACUM_RES1
                                    ENDIF
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
                        UNLOCK
                     ENDIF
                  ENDIF
                  SELE 8
                  SKIP
               ENDDO
               SELE 6
               GOTO TOP
               SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
               IF F_BLOQR()
                  REPLACE ULT_CORREC WITH CTOD("01/"+eMES_LIMIT+"/"+eANO_LIMIT)
                  UNLOCK
               ENDIF
               SKIP
            ELSE
               SELE 6
               SKIP
            ENDIF

RETURN nil
*************************  fim  *********************************
