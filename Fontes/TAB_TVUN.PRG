*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_TVUN.PRG
* Data        : 01/01/99
* Objetivo    : CORRECAO DE VALOR DE UNIDADES
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function TAB_TVUN()
SAVE SCREEN TO TELA03
PTIT( "CORRECAO DE VALORES DE UNIDADES")
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
USE TIPO_PRO INDEX TIPO_PRO
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
   eQUADRA   = SPACE(1)
   eQUADRA   = SPACE(1)
   EXIT
ENDDO
* Selecao 
DO CASE
   CASE eTIPO_PRO = 1
      * Relatorio de Loteamentos
      PLQUA("01")
      PPERG ( "Confirma a Correcao (S/N) ? ")
      IF sisRESP = "S"
         ePERCENTUAL = 0
         PLQUA("01")
         @ 23,00 SAY "Informe o Percentual de Aumento : " GET ePERCENTUAL PICT "@R 999"
         READ
         IF ePERCENTUAL <> 0
            PLQUA("01")
            PPERG ( "Confirma a Correcao (S/N) ? ")
            IF sisRESP = "S"
               PLQUA("01")
			   @ 23,00 SAY "Aguarde o processamento ..."
               SELE 1
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5)
               GOTO TOP
               DO WHILE .NOT. EOF()
                  IF F_BLOQR()
                     eVALOR_1 = 0
                     eVALOR_2 = 0
                     eVALOR_1 = VALOR_1 * ((ePERCENTUAL/100)+1)
                     eVALOR_2 = VALOR_2 * ((ePERCENTUAL/100)+1)
                     REPLACE VALOR_1 WITH eVALOR_1
                     REPLACE VALOR_2 WITH eVALOR_2
                     UNLOCK
                  ENDIF
                  SKIP
                  @ 23,00 SAY "Aguarde o processamento  . "
               ENDDO
            ENDIF
         ENDIF
      ENDIF
   CASE eTIPO_PRO = 2
      * Relatorio de Apartamentos
      PLQUA("01")
      PPERG ( "Confirma a Correcao (S/N) ? ")
      IF sisRESP = "S"
         ePERCENTUAL = 0
         PLQUA("01")
         @ 23,00 SAY "Informe o Percentual de Aumento : " GET ePERCENTUAL PICT "@R 999"
         READ
         IF ePERCENTUAL <> 0
            PLQUA("01")
            PPERG ( "Confirma a Correcao (S/N) ? ")
            IF sisRESP = "S"
               PLQUA("01")
			   @ 23,00 SAY "Aguarde o processamento ..."
               SELE 1
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5)
               GOTO TOP
               DO WHILE .NOT. EOF()
                  IF F_BLOQR()
                     eVALOR_1 = 0
                     eVALOR_2 = 0
                     eVALOR_1 = VALOR_1 * ((ePERCENTUAL/100)+1)
                     eVALOR_2 = VALOR_2 * ((ePERCENTUAL/100)+1)
                     REPLACE VALOR_1 WITH eVALOR_1
                     REPLACE VALOR_2 WITH eVALOR_2
                     UNLOCK
                  ENDIF
                  SKIP
                  @ 23,00 SAY "Aguarde o processamento  . "
               ENDDO
            ENDIF
         ENDIF
      ENDIF
   OTHERWISE
      * Relatorio de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
