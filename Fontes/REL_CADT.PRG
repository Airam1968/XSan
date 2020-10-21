*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_CADT.PRG
* Data        : 01/01/99
* Objetivo    : IMPRESSAO DA RELATORIO CADASTRO DE CLIENTES TOTAL
* PRG's       : ---
* DBF's       : ---
* Alteracao   :
************************* inicio ************************************
DO PPERG WITH 'Confirma (S/N) ?'
IF sisRESP = 'S'
   DO PTIT WITH 'RELATORIO CADASTRO TOTAL'
   DO PLQUA WITH '01'
   DO PPERG WITH 'Deseja Relatorio com Observacoes Gerais (S/N) ? '
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
       
   DO PMENSAG WITH 'Posicione o Formulario 80 Colunas e Tecle [ENTER]'
   DO PPARA
   SELECT 1
   USE CONTATO INDEX CONTATO,NUM_CLIE
   GO TOP
*  SKIP
   IF EOF()
      PLQUA("01")
      PMSGR( "Atená∆o !",3)
      PMENSAG("N∆o H† Relat¢rio para esta Seleá∆o !")
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
      @ 02,115 SAY 'Relat: SAN002'
      @ 03,40 SAY sisLETRAE+'CADASTRO DE CLIENTES COMPLETO'+sisLETRAC
      @ 04,00 SAY REPLICATE('=',135)
      @ 05,02  SAY 'NOME'
      @ 05,52  SAY 'CPF/CGC'
      @ 05,70  SAY 'RG'
      @ 05,88  SAY 'UF RG'
      @ 05,96  SAY 'EMISSAO RG'
      @ 05,107 SAY 'ORGAO RG'
      @ 05,117 SAY 'NASCIMENTO'
      @ 05,129 SAY 'SEXO'
      @ 06,02  SAY 'ENDERECO Res'
      @ 06,44  SAY 'BAIRRO Res'
      @ 06,74  SAY 'CIDADE Res'
      @ 06,114  SAY 'ESTADO Res'
      @ 07,34  SAY 'CEP Res'
      @ 07,44  SAY 'TEL 1 Res'
      @ 07,60  SAY 'TEL 2 Res'
      @ 08,02  SAY 'ENDERECO Com'
      @ 08,44  SAY 'BAIRRO Com'
      @ 08,74  SAY 'CIDADE Com'
      @ 08,114  SAY 'ESTADO Com'
      @ 09,34  SAY 'CEP Com'
      @ 09,44  SAY 'TEL 1 Com'
      @ 09,60  SAY 'TEL 2 Com'
      @ 10,00 SAY REPLICATE('=',135)
      LIN = 11
      DO WHILE .NOT. EOF() .AND. LIN <= 52
         @ LIN,01  SAY ALLTRIM(CLIENTE)
         @ LIN,52  SAY CPF_CGC
         @ LIN,70  SAY RG
         @ LIN,88  SAY UF_RG
         @ LIN,96  SAY EMISSAO_RG
         @ LIN,107 SAY ORGAO_RG
         @ LIN,117 SAY NASCIMENTO
         @ LIN,130 SAY SEXO
         LIN = LIN + 1
         @ LIN,01  SAY END_R
         @ LIN,44  SAY BAIRRO_R
         @ LIN,74  SAY CIDADE_R
         @ LIN,114 SAY ESTADO_R
         LIN = LIN + 1
         @ LIN,34  SAY CEP_R PICT '@R 99999-999'
         @ LIN,44  SAY TEL_R_1 PICT '@R (XX) XXXXX-XXXXX'
         @ LIN,60  SAY TEL_R_2 PICT '@R (XX) XXXXX-XXXXX'
         LIN = LIN + 1
         @ LIN,01  SAY END_C
         @ LIN,44  SAY BAIRRO_C
         @ LIN,74  SAY CIDADE_C
         @ LIN,114 SAY ESTADO_C
         LIN = LIN + 1
         @ LIN,34  SAY CEP_C PICT '@R 99999-999'
         @ LIN,44  SAY TEL_C_1 PICT '@R (XX) XXXXX-XXXXX'
         @ LIN,60  SAY TEL_C_2 PICT '@R (XX) XXXXX-XXXXX'
         LIN = LIN + 1
         IF sisRESP = 'S'
            @ LIN,01  SAY OBSERVA_1
            LIN = LIN + 1
            @ LIN,01  SAY OBSERVA_2
            LIN = LIN + 1
            @ LIN,01  SAY OBSERVA_3
            LIN = LIN + 1
            @ LIN,01  SAY OBSERVA_4
            LIN = LIN + 1
            @ LIN,01  SAY OBSERVA_5
            LIN = LIN + 1
         ENDIF
         @ LIN,00 SAY REPLICATE('-',135)
         LIN = LIN + 1
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
      @ 57,00 SAY REPLICATE('=',135)
      @ 58,95 SAY sisTIT3
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
CLOSE ALL
RETURN
***************************** fim ***********************************
