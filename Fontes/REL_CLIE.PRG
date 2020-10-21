*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_CLIE.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO CADASTRO DE CLIENTES POR EMPREENDIMENTO
* PRG's       : ---
* DBF's       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_CLIE()
DO PPERG WITH 'Confirma (S/N) ?'
IF sisRESP = 'S'
   DO PTIT WITH 'RELATORIO CADASTRO DE CLIENTES POR PRODUTO'
   DO PLQUA WITH '01'
   eTIPO = SPACE(1)
   @ 23,02 SAY 'Ordem Alfab‚tica ou Num‚rica (A/N)  : ' GET eTIPO PICT 'X' VALID eTIPO $('AN')
   READ
   eEMPREEND = 0
   @ 23,02 SAY 'Informe o N£mero do Empreendimento ou (0) para todos : ' GET eEMPREEND PICT '@R 9999'
   READ
   eARQUIVO = SPACE(60)
   IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
   ELSE
	eARQUIVO = "C:"
   ENDIF	
   IF sisPASTA_REL <> SPACE(30)
	eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"EXTRATO.TXT"+SPACE(10)  
   ELSE
	eARQUIVO = ALLTRIM(eARQUIVO) + "EXTRATO.TXT"+SPACE(10) 
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
         eSAIDA = "ARQUIVO"
         EXIT
      ELSE
         DO PIMPR
         eSAIDA = "IMPRESSORA"
         EXIT
      ENDIF
   ENDDO      
   IF eTIPO = 'A'
      SELECT 1
      USE CONTATO INDEX CONTATO,NUM_CLIE
      SET FILTER TO NUM_CLIE <> 1
      GO TOP
      SKIP
   ELSE
      SELECT 1
      USE CONTATO INDEX NUM_CLIE,CONTATO
      SET FILTER TO NUM_CLIE <> 1
      GO TOP
      SKIP
   ENDIF
   PLQUA("01")
   PMENSAG("Indexando Vendas !")   
   SELE 2
   USE UNI_IMOB
   GOTO TOP
   INDEX ON PROPRIET_1 TO UNI_IMO1
   GOTO TOP
   INDEX ON PROPRIET_2 TO UNI_IMO2
   GOTO TOP
   USE UNI_IMOB INDEX UNI_IMO1,UNI_IMO2
   GOTO TOP
   IF eEMPREEND <> 0
      SET FILTER TO CLASSE = STR(eEMPREEND,5)
      GOTO TOP
   ENDIF
   SELE 2
   GOTO TOP
   IF EOF()
      PLQUA("01")
      PMSGR( "Aten‡Æo !",3)
      PMENSAG("NÆo H  Relat¢rio Deste Per¡odo !")
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
   ENDIF   
   DO WHILE .NOT. EOF()
      @ 01,00 SAY sisLETRAC+'Empresa : '+sisTIT1
      @ 01,115 SAY 'Data : '+dtoc(DATE())
      @ 02,00 SAY 'Sistema : '+sisTIT2
      @ 02,115 SAY 'Relat: SAN001'
      @ 03,40 SAY sisLETRAE+'CADASTRO DE CLIENTES'+sisLETRAC
      @ 04,00 SAY REPLICATE('=',135)
      @ 05,00  SAY '|'
      @ 05,02  SAY 'Codigo'
      @ 05,10  SAY 'Nome'
      @ 05,50  SAY 'Produto'            && Era Empreend.
      @ 05,60  SAY 'Grupo'              && Era Quad/Bloc
      @ 05,70  SAY 'Unid.'              && Era Apart.
      @ 05,80  SAY 'Parte'
      @ 05,90  SAY 'Telefone'
      @ 05,134 SAY '|'
      @ 06,00 SAY REPLICATE('=',135)
      LIN = 7
      DO WHILE .NOT. EOF() .AND. LIN <= 54
         DO WHILE .T.
            SELE 1
            eNUM_CLIE = NUM_CLIE
            SELE 2
            SET ORDER TO 1
            GOTO TOP
            SEEK eNUM_CLIE
            IF .NOT. FOUND()
               SELE 2
               SET ORDER TO 2
               GOTO TOP
               SEEK eNUM_CLIE
               IF .NOT. FOUND()
                  SELE 1
                  SKIP
                  LOOP
               ELSE
                  SELE 1
                  EXIT
               ENDIF
            ELSE
               EXIT
            ENDIF
         ENDDO
         DO WHILE .T.
            SELE 2
            IF PROPRIET_1 = eNUM_CLIE 
               SELE 1
               @ LIN,00  SAY '|'
               @ LIN,01  SAY NUM_CLIE PICT '@R 99999999'
               @ LIN,10  SAY ALLTRIM(CLIENTE) PICT '@S39'
               IF OPCAO = 'R'
                  eTEL1 = TEL_R_1
                  eTEL2 = TEL_R_2
               ELSE
                  eTEL1 = TEL_C_1
                  eTEL2 = TEL_c_2
               ENDIF
               SELE 2
               @ LIN,50  SAY CLASSE     PICT '@R 99999'
               @ LIN,60  SAY QUADRA     PICT '@R 99999'
               @ LIN,70  SAY NUM_IMOVEL PICT '@R 99999'
               @ LIN,80  SAY IIF(STATUS = 0,0,1) PICT '@R 9'
               @ LIN,85  SAY eTEL1 PICT '@R (XX) XXXXX-XXXXX'
               @ LIN,105 SAY eTEL2 PICT '@R (XX) XXXXX-XXXXX'
               @ LIN,134 SAY '|'
               LIN  = LIN + 1
               @ LIN,00  SAY '|'
               SELE 1
               IF OPCAO = 'R'
                  @ LIN,15  SAY 'RESIDENCIAL: '+ALLTRIM(END_R)+' - '+ALLTRIM(BAIRRO_R)+' - ';
                  +ALLTRIM(CIDADE_R)+' - '+ALLTRIM(ESTADO_R)+' - '+SUBSTR(CEP_R,1,5)+'-'+SUBSTR(CEP_R,6,3)
               ELSE
                  @ LIN,15  SAY 'COMERCIAL  : '+ALLTRIM(END_C)+' - '+ALLTRIM(BAIRRO_C)+' - ';
                  +ALLTRIM(CIDADE_C)+' - '+ALLTRIM(ESTADO_C)+' - '+SUBSTR(CEP_C,1,5)+'-'+SUBSTR(CEP_C,6,3)
               ENDIF
               @ LIN,134 SAY '|'
               LIN  = LIN + 1
               @ LIN,00  SAY '|'
               SELE 1
               @ LIN,15  SAY 'E-MAIL PROP: '+ALLTRIM(MAIL_PROP)
               @ LIN,134 SAY '|'
               SELE 2
               SKIP
               LIN = LIN + 1
            ELSE
               IF PROPRIET_2 = eNUM_CLIE
                  SELE 1
                  @ LIN,00  SAY '|'
                  @ LIN,01  SAY NUM_CLIE PICT '@R 99999999'
                  @ LIN,10  SAY ALLTRIM(CLIENTE) PICT '@S39'
                  IF OPCAO = 'R'
                     eTEL1 = TEL_R_1
                     eTEL2 = TEL_R_2
                  ELSE
                     eTEL1 = TEL_C_1
                     eTEL2 = TEL_c_2
                  ENDIF
                  SELE 2
                  @ LIN,50  SAY CLASSE     PICT '@R 99999'
                  @ LIN,60  SAY QUADRA     PICT '@R 99999'
                  @ LIN,70  SAY NUM_IMOVEL PICT '@R 99999'
                  @ LIN,80  SAY '2'
                  @ LIN,85  SAY eTEL1 PICT '@R (XX) XXXXX-XXXXX'
                  @ LIN,105 SAY eTEL2 PICT '@R (XX) XXXXX-XXXXX'
                  @ LIN,134 SAY '|'
                  LIN  = LIN + 1
                  @ LIN,00  SAY '|'
                  SELE 1
                  IF OPCAO = 'R'
                     @ LIN,15  SAY 'RESIDENCIAL: '+ALLTRIM(END_R)+' - '+ALLTRIM(BAIRRO_R)+' - ';
                     +ALLTRIM(CIDADE_R)+' - '+ALLTRIM(ESTADO_R)+' - '+SUBSTR(CEP_R,1,5)+'-'+SUBSTR(CEP_R,6,3)
                  ELSE
                     @ LIN,15  SAY 'COMERCIAL  : '+ALLTRIM(END_C)+' - '+ALLTRIM(BAIRRO_C)+' - ';
                     +ALLTRIM(CIDADE_C)+' - '+ALLTRIM(ESTADO_C)+' - '+SUBSTR(CEP_C,1,5)+'-'+SUBSTR(CEP_C,6,3)
                  ENDIF
                  @ LIN,134 SAY '|'
                  LIN  = LIN + 1
                  @ LIN,00  SAY '|'
                  SELE 1
                  @ LIN,15  SAY 'E-MAIL PROP: '+ALLTRIM(MAIL_PROP)
                  @ LIN,134 SAY '|'
                  SELE 2
                  SKIP
                  LIN = LIN + 1
                  DO PTECLA
                  IF sisINTERR
                     SET DEVICE TO SCREEN
                     CLOSE ALL
                     RETURN
                  ENDIF
               ENDIF
            ENDIF
            SELE 2
            IF (PROPRIET_1 <> eNUM_CLIE .AND. PROPRIET_2 <> eNUM_CLIE) .OR. EOF()
               SELE 1
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
               EXIT
            ENDIF
            IF LIN >= 54
               @ 58,00 SAY REPLICATE('=',135)
               @ 59,104 SAY sisTIT3
               @ 01,00 SAY ' '+sisLETRAN
               @ 01,00 SAY sisLETRAC+'Empresa : '+sisTIT1
               @ 01,115 SAY 'Data : '+dtoc(DATE())
               @ 02,00 SAY 'Sistema : '+sisTIT2
               @ 02,115 SAY 'Relat: SAN001'
               @ 03,40 SAY sisLETRAE+'CADASTRO DE CLIENTES'+sisLETRAC
               @ 04,00 SAY REPLICATE('=',135)
               @ 05,00  SAY '|'
               @ 05,02  SAY 'Codigo'
               @ 05,10  SAY 'Nome'
               @ 05,50  SAY 'Produto.'           && Era Empreend.
               @ 05,60  SAY 'Grupo'              && Era Quad/Bloc
               @ 05,70  SAY 'Unid.'              && Era Imovel
               @ 05,80  SAY 'Parte'
               @ 05,134 SAY '|'
               @ 06,00 SAY REPLICATE('=',135)
               LIN = 7
            ENDIF
         ENDDO
      ENDDO
      @ 56,00 SAY REPLICATE('=',135)
      @ 57,104 SAY sisTIT3
      @ 01,00 SAY ' '+sisLETRAN
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
CLOSE ALL
RETURN nil
***************************** fim ***********************************

