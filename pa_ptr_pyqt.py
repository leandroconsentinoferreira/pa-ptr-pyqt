import csv
import os
import sys

from PySide2.QtWidgets import *
import pandas as pd
import chardet

from ui_pa_ptr_pyqt import Ui_MainWindow

import numpy as np
# import random


# https://pythonprogramming.net/file-saving-pyqt-tutorial/
# pyinstaller pa_ptr_pyqt.spec
# df.loc[(df['Grupo'] == 'SHR 17 Semanas Tratados') & (df['Dosagem'] == 'PBS'), 'Ptr_cmH2O']
# df.to_csv(caminho_w,sep=';',index=False)
# se len(df filtrado) = 0
# https://pythonexamples.org/pandas-dataframe-add-append-row/

global df_saida, arquivo_dados_brutos_pa, df_ptr, \
    arquivo_dados_brutos_ptr, arquivo_parametros, arquivo_saida, tx_animal_pa, tx_animal_ptr


class Window(QMainWindow, Ui_MainWindow):

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setupUi(self)

        self.btArquivoBruto_3.released.connect(self.busca_arquivo_dados_brutos_pa)
        self.btArquivoBruto_4.released.connect(self.busca_arquivo_dados_brutos_ptr)

        self.btArquivoBruto_5.released.connect(self.busca_arquivo_dados_brutos_pa_2)
        self.btArquivoBruto_6.released.connect(self.busca_arquivo_dados_brutos_ptr_2)

        self.btParametros_3.released.connect(self.busca_arquivo_parametros)
        self.btParametros_4.released.connect(self.busca_arquivo_parametros_2)

        self.btArquivoSaida_3.released.connect(self.busca_arquivo_saida)
        self.btArquivoSaida_4.released.connect(self.busca_arquivo_saida)
        self.btCarregar_3.released.connect(self.carrega_dados_pa)
        self.btCarregar_4.released.connect(self.carrega_dados_ptr)

        self.btCarregar_5.released.connect(self.carrega_dados_pa_2)
        self.btCarregar_6.released.connect(self.carrega_dados_ptr_2)

        self.btLimpar_3.released.connect(self.limpa_campos_pa)
        self.btLimpar_4.released.connect(self.limpa_campos_ptr)

        self.btLimpar_5.released.connect(self.limpa_campos_pa_2)
        self.btLimpar_6.released.connect(self.limpa_campos_ptr_2)

        self.btLimpar_7.released.connect(self.limpa_graficos_pa)
        self.btLimpar_8.released.connect(self.limpa_graficos_ptr)

        self.btAtualizar_3.released.connect(self.atualiza_dados_pa)
        self.btAtualizar_4.released.connect(self.atualiza_dados_ptr)

        self.btAtualizar_5.released.connect(self.atualiza_dados_pa_2)
        self.btAtualizar_7.released.connect(self.atualiza_dados_ptr_2)

        self.btInicioPa_3.released.connect(self.detecta_inicio_pa)
        self.btSalvar_3.released.connect(self.salva_arquivo_pa)
        self.btSalvar_4.released.connect(self.salva_arquivo_ptr)
        self.label_5.setText('')
        self.actionAjuda.triggered.connect(self.abre_manual)
        self.actionSobre.triggered.connect(self.show_popup_sobre)

        self.cbGrupoAnimais_3.clear()

        df_lista_grupos = pd.read_csv('resources\\grupos.csv')
        lista_grupos = df_lista_grupos['Grupo'].astype('str').values.tolist()

        for grupo in lista_grupos:
            self.cbGrupoAnimais_3.addItem(grupo)
            self.cbGrupoAnimais_4.addItem(grupo)
            self.cbGrupoAnimais_5.addItem(grupo)
            self.cbGrupoAnimais_6.addItem(grupo)

    def abre_manual(self):
        print('abre manual')
        os.startfile('resources\\Manual rev01.pdf')

    def onclick(self, event):
        global medias_trechos_, desvios_trechos_
        medias_trechos_ = []
        desvios_trechos_ = []
        # print('%s click: button=%d, x=%d, y=%d, xdata=%f, ydata=%f' %
        #       ('double' if event.dblclick else 'single', event.button,
        #        event.x, event.y, event.xdata, event.ydata))
        inicio_intra_, fim_intra_, inicio_inter_, fim_inter_ = self.identifica_perturbacoes(df_tukey, event.xdata)
        # print('Inicio intra, fim, etc')
        # print(inicio_intra_, fim_intra_, inicio_inter_, fim_inter_)
        self.MplWidget_3.canvas.axes2.plot(df_tukey.Time[inicio_inter_], df_tukey.PA[inicio_inter_], 'oy', linewidth=0.5)
        self.MplWidget_3.canvas.axes2.plot(df_tukey.Time[fim_inter_], df_tukey.PA[fim_inter_], 'xy', linewidth=0.5)
        self.MplWidget_3.canvas.axes2.plot(df_tukey.Time[inicio_intra_], df_tukey.PA[inicio_intra_], 'or', linewidth=0.5)
        self.MplWidget_3.canvas.axes2.plot(df_tukey.Time[fim_intra_], df_tukey.PA[fim_intra_], 'xr', linewidth=0.5)
        self.MplWidget_3.canvas.draw()
        self.MplWidget_3.canvas.mpl_disconnect(cid)
        medias_trechos_, desvios_trechos_ = self.calcula_dados_perturbacoes(inicio_intra_, fim_intra_)
        print(medias_trechos_, desvios_trechos_)

    def salva_arquivo_pa(self):
        contador_perturbacao = 1
        busca_existe_ptr = []
        busca_existe_pa = []
        arquivo_saida = self.txArquivoSaida_3.text()
        dosagem = self.cbEtapas_3.currentText()
        dosagem_corrigida = dosagem.split(' - ')[1]
        if arquivo_saida == '':
            print('sem arquivo saida')
        else:
            try:
                df_arquivo = pd.read_csv(arquivo_saida, sep=';')

                busca_existe_pa = df_arquivo.loc[(df_arquivo['Grupo'] == self.cbGrupoAnimais_3.currentText()) &
                                                 (df_arquivo['Dosagem'] == dosagem_corrigida) &
                                                 (df_arquivo['Animal'] == int(self.txAnimal_3.text())), 'PA_mmHg']

                busca_existe_ptr = df_arquivo.loc[(df_arquivo['Grupo'] == self.cbGrupoAnimais_3.currentText()) &
                                                  (df_arquivo['Dosagem'] == dosagem_corrigida) &
                                                  (df_arquivo['Animal'] == int(self.txAnimal_3.text())), 'Ptr_cmH2O']

                print(busca_existe_pa, busca_existe_ptr)
                print(len(busca_existe_pa), len(busca_existe_ptr))
                print(dosagem_corrigida)

                if len(busca_existe_pa) or len(busca_existe_ptr):
                    for media_pa, desvio_pa in zip(medias_trechos_, desvios_trechos_):
                        df_arquivo.loc[(df_arquivo['Grupo'] == self.cbGrupoAnimais_3.currentText()) &
                                       (df_arquivo['Dosagem'] == dosagem_corrigida) &
                                       (df_arquivo['Animal'] == int(self.txAnimal_3.text())) &
                                       (df_arquivo['Perturbacao'] == contador_perturbacao), 'PA_mmHg'] = media_pa

                        df_arquivo.loc[(df_arquivo['Grupo'] == self.cbGrupoAnimais_3.currentText()) &
                                       (df_arquivo['Dosagem'] == dosagem_corrigida) &
                                       (df_arquivo['Animal'] == int(self.txAnimal_3.text())) &
                                       (df_arquivo['Perturbacao'] == contador_perturbacao), 'Desvio_PA'] = desvio_pa

                        contador_perturbacao += 1
                else:
                    for media_pa, desvio_pa in zip(medias_trechos_, desvios_trechos_):
                        nova_linha_pa = {'Grupo': self.cbGrupoAnimais_3.currentText(),
                                         'Animal': int(self.txAnimal_3.text()),
                                         'Dosagem': dosagem_corrigida,
                                         'Perturbacao': contador_perturbacao,
                                         'PA_mmHg': media_pa,
                                         'Desvio_PA': desvio_pa,
                                         'Ptr_cmH2O': '',
                                         'Desvio_Ptr': ''}

                        contador_perturbacao += 1

                        df_arquivo = df_arquivo.append(nova_linha_pa, ignore_index=True)

                # print(self.cbGrupoAnimais_3.currentText(), self.txAnimal_3.text())
                df_arquivo.to_csv(arquivo_saida, sep=';', index=False)
                # print(grupo, animal)
                # print(type(self.cbGrupoAnimais_3.currentText()), type(self.txAnimal_3.text()))
                # print(a)
            except FileNotFoundError:
                print('arquivo inexistente, criando novo')
                cabecalho = ['Grupo', 'Animal', 'Dosagem', 'Perturbacao', 'PA_mmHg',
                             'Desvio_PA', 'Ptr_cmH2O', 'Desvio_Ptr']
                # procurar help do to_csv
                df_arquivo = pd.DataFrame([], columns=cabecalho)
                df_arquivo.to_csv(arquivo_saida, sep=';', index=False, )
                self.salva_arquivo_pa()

    def salva_arquivo_ptr(self):
        arquivo_saida = self.txArquivoSaida_4.text()
        busca_existe_ptr = []
        busca_existe_pa = []

        dosagem = self.cbEtapas_4.currentText()
        dosagem_corrigida = dosagem.split(' - ')[1]

        contador_perturbacao = 1
        if arquivo_saida == '':
            print('sem arquivo saida')
        else:
            try:
                df_arquivo = pd.read_csv(arquivo_saida, sep=';')

                busca_existe_pa = df_arquivo.loc[(df_arquivo['Grupo'] == self.cbGrupoAnimais_4.currentText()) &
                                                 (df_arquivo['Dosagem'] == dosagem_corrigida) &
                                                 (df_arquivo['Animal'] == int(self.txAnimal_4.text())), 'PA_mmHg']

                busca_existe_ptr = df_arquivo.loc[(df_arquivo['Grupo'] == self.cbGrupoAnimais_4.currentText()) &
                                                  (df_arquivo['Dosagem'] == dosagem_corrigida) &
                                                  (df_arquivo['Animal'] == int(self.txAnimal_4.text())), 'Ptr_cmH2O']

                print(busca_existe_pa, busca_existe_ptr)
                print(len(busca_existe_pa), len(busca_existe_ptr))
                print(self.cbGrupoAnimais_4.currentText())
                print(dosagem_corrigida)
                print(int(self.txAnimal_4.text()))

                if len(busca_existe_pa) or len(busca_existe_ptr):
                    for media_ptr, desvio_ptr in zip(medias_ptr, desvios_ptr):
                        df_arquivo.loc[(df_arquivo['Grupo'] == self.cbGrupoAnimais_4.currentText()) &
                                       (df_arquivo['Dosagem'] == dosagem_corrigida) &
                                       (df_arquivo['Animal'] == int(self.txAnimal_4.text())) &
                                       (df_arquivo['Perturbacao'] == contador_perturbacao), 'Ptr_cmH2O'] = media_ptr

                        df_arquivo.loc[(df_arquivo['Grupo'] == self.cbGrupoAnimais_4.currentText()) &
                                       (df_arquivo['Dosagem'] == dosagem_corrigida) &
                                       (df_arquivo['Animal'] == int(self.txAnimal_4.text())) &
                                       (df_arquivo['Perturbacao'] == contador_perturbacao), 'Desvio_Ptr'] = desvio_ptr

                        contador_perturbacao += 1
                else:
                    for media_ptr, desvio_ptr in zip(medias_ptr, desvios_ptr):
                        nova_linha_ptr = {'Grupo': self.cbGrupoAnimais_4.currentText(),
                                          'Animal': int(self.txAnimal_4.text()),
                                          'Dosagem': dosagem_corrigida,
                                          'Perturbacao': contador_perturbacao,
                                          'PA_mmHg': '',
                                          'Desvio_PA': '',
                                          'Ptr_cmH2O': media_ptr,
                                          'Desvio_Ptr': desvio_ptr}

                        contador_perturbacao += 1

                        df_arquivo = df_arquivo.append(nova_linha_ptr, ignore_index=True)

                # print(self.cbGrupoAnimais_3.currentText(), self.txAnimal_3.text())
                df_arquivo.to_csv(arquivo_saida, sep=';', index=False)

            except FileNotFoundError:
                print('arquivo inexistente, criando novo')
                cabecalho = ['Grupo', 'Animal', 'Dosagem', 'Perturbacao', 'PA_mmHg',
                             'Desvio_PA', 'Ptr_cmH2O', 'Desvio_Ptr']
                # procurar help do to_csv
                df_arquivo = pd.DataFrame([], columns=cabecalho)
                df_arquivo.to_csv(arquivo_saida, sep=';', index=False, )
                self.salva_arquivo_ptr()

    def carrega_dados_pa(self):
        global df, etapas
        df = []
        etapas = []
        # print(arquivo_dados_brutos_pa)

        arquivo_dados_brutos_pa = self.txArquivoBruto_3.text()
        arquivo_parametros = self.txParametros_3.text()
        arquivo_saida = self.txArquivoSaida_3.text()
        tx_animal_pa = self.txAnimal_3.text()

        if arquivo_dados_brutos_pa == '' or arquivo_parametros == '' or arquivo_saida == '' or tx_animal_pa == '':
            self.show_popup('Preencha os campos!')
            return

        try:
            df = pd.read_csv(arquivo_dados_brutos_pa, delimiter=",")
        except FileNotFoundError:
            self.show_popup('Arquivo PA não encontrado!')
            return
        except pd.errors.ParserError:
            df = pd.read_csv(arquivo_dados_brutos_pa, delimiter=",", skiprows=12).dropna()
        df.columns = ['Time', 'PA', 'AN']
        try:
            etapas = pd.read_excel(arquivo_parametros, usecols="A,B", engine="odf")
        except FileNotFoundError:
            self.show_popup('Arquivo de Etapas não encontrado!')
            return

        etapas.columns = ['Oc', 'AN']
        action_numbers = pd.unique(df.AN)

        self.MplWidget_3.canvas.axes1.clear()
        self.MplWidget_3.canvas.axes2.clear()
        self.cbEtapas_3.clear()

        for action_number in action_numbers:
            # print(action_number)
            df2 = df[df['AN'] == action_number]
            texto = '{an} - {et}'.format(an=action_number, et=etapas.Oc[action_number])
            self.MplWidget_3.canvas.axes1.plot(df2.Time, df2.PA, linewidth=0.5, label=texto)
            self.cbEtapas_3.addItem(texto)
        self.MplWidget_3.canvas.axes1.legend(loc='best', prop={'size': 6})
        self.MplWidget_3.canvas.axes1.set_title('Sinal Completo Pressão Arterial', fontsize=10)
        self.MplWidget_3.canvas.axes1.set_ylabel('PA (mmHg)')
        self.MplWidget_3.canvas.draw()

    def carrega_dados_pa_2(self):
        global df, etapas
        df = []
        etapas = []
        # print(arquivo_dados_brutos_pa)

        arquivo_dados_brutos_pa_2 = self.txArquivoBruto_5.text()
        arquivo_parametros_2 = self.txParametros_4.text()
        tx_animal_pa_2 = self.txAnimal_5.text()

        if arquivo_dados_brutos_pa_2 == '' or arquivo_parametros_2 == '' or tx_animal_pa_2 == '':
            self.show_popup('Preencha os campos!')
            return

        try:
            df = pd.read_csv(arquivo_dados_brutos_pa_2, delimiter=",")
        except FileNotFoundError:
            self.show_popup('Arquivo PA não encontrado!')
            return
        except pd.errors.ParserError:
            df = pd.read_csv(arquivo_dados_brutos_pa_2, delimiter=",", skiprows=12).dropna()
        df.columns = ['Time', 'PA', 'AN']
        try:
            etapas = pd.read_excel(arquivo_parametros_2, usecols="A,B", engine="odf")
        except FileNotFoundError:
            self.show_popup('Arquivo de Etapas não encontrado!')
            return

        etapas.columns = ['Oc', 'AN']
        action_numbers = pd.unique(df.AN)

        self.MplWidget3.canvas3.axes1.clear()
        # self.MplWidget3.canvas3.axes2.clear()
        self.cbEtapas_5.clear()

        for action_number in action_numbers:
            # print(action_number)
            df2 = df[df['AN'] == action_number]
            texto = '{an} - {et}'.format(an=action_number, et=etapas.Oc[action_number])
            self.MplWidget3.canvas3.axes1.plot(df2.Time, df2.PA, linewidth=0.5, label=texto)
            self.cbEtapas_5.addItem(texto)
        self.MplWidget3.canvas3.axes1.legend(loc='best', prop={'size': 6})
        self.MplWidget3.canvas3.axes1.set_title('Sinal Completo Pressão Arterial', fontsize=10)
        self.MplWidget3.canvas3.axes1.set_ylabel('PA (mmHg)')
        self.MplWidget3.canvas3.draw()

    def carrega_dados_ptr(self):
        global df, df_ptr, lista_dosagens
        df = []
        df_ptr = []

        arquivo_dados_brutos_ptr = self.txArquivoBruto_4.text()
        arquivo_saida = self.txArquivoSaida_4.text()
        tx_animal_ptr = self.txAnimal_4.text()

        if arquivo_dados_brutos_ptr == '' or arquivo_saida == '' or tx_animal_ptr == '':
            self.show_popup('Preencha os campos!')
            return

        padding = 0

        lista_dosagens = ['PBS', '3 ug/kg', '10 ug/kg', '30 ug/kg', '100 ug/kg', '300 ug/kg', 'N/A']
        try:
            rawdata = open(arquivo_dados_brutos_ptr, 'rb').read()
            res = chardet.detect(rawdata)
        except FileNotFoundError:
            self.show_popup('Arquivo Ptr não encontrado!')
            return

        if res['encoding'] == 'UTF-16':  # flexiVent novo
            self.label_5.setText('flexiWare')
            # print('flexiVent novo')
            lista_skip = list(range(0, 13)) + [14]
            df = pd.read_csv(arquivo_dados_brutos_ptr, sep='\\t', header=0, error_bad_lines=False,
                             encoding='utf-16', skiprows=lista_skip, engine='python')
        else:  # flexiVent Antigo
            self.label_5.setText('Legacy')
            padding = 1
            # print('flexiVent antigo')
            lista_skip = list(range(0, 21)) + [22]
            df = pd.read_csv(arquivo_dados_brutos_ptr, sep='\\t', header=0, error_bad_lines=False,
                             encoding='utf-8', skiprows=lista_skip, engine='python')

        print(df)
        self.MplWidget2.canvas2.axes1.clear()
        self.MplWidget2.canvas2.axes2.clear()
        self.MplWidget2.canvas2.axes3.clear()
        self.MplWidget2.canvas2.axes4.clear()
        self.cbEtapas_4.clear()

        coluna_inicio = 0
        contador_coluna = 0

        diff_s = np.diff(df.Time)
        step_tempo = np.mean(diff_s)

        for column_name in df.columns:  # Verifica quando dosagens começam
            if 'Ptr' in column_name:
                # print(column_name)
                if pd.Series.value_counts(df[column_name]).__len__() > 780:
                    coluna_inicio = contador_coluna + 1
            contador_coluna += 1

        print(coluna_inicio)

        ptr = []
        vtr = []
        action_number_ptr = []
        action_number_vtr = []
        contador_perturbacoes_ptr = 0
        contador_perturbacoes_vtr = 0
        perturbacao_ptr = 0
        perturbacao_vtr = 0
        for column_name in df.columns[coluna_inicio:]:
            if 'Ptr' in column_name:
                ptr_limpa = df[column_name]  # .dropna()
                ptr.extend(ptr_limpa)  # .values)
                if padding:
                    ptr.extend([np.nan] * 769)
                action_number_ptr.extend([perturbacao_ptr] * (len(ptr_limpa) + (padding * 769)))
                if contador_perturbacoes_ptr != 0 and contador_perturbacoes_ptr % 14 == 0:
                    perturbacao_ptr += 1
                    contador_perturbacoes_ptr = 0
                else:
                    contador_perturbacoes_ptr += 1

            if 'Vtr' in column_name:
                vtr_limpa = df[column_name]  # .dropna()
                vtr.extend(vtr_limpa)  # .values)
                if padding:
                    vtr.extend([np.nan] * 769)
                action_number_vtr.extend([perturbacao_vtr] * (len(vtr_limpa) + (padding * 769)))
                if contador_perturbacoes_vtr != 0 and contador_perturbacoes_vtr % 14 == 0:
                    perturbacao_vtr += 1
                    contador_perturbacoes_vtr = 0
                else:
                    contador_perturbacoes_vtr += 1

        tempo_ptr = np.linspace(0, len(ptr)*step_tempo, len(ptr))
        # tempo_vtr = np.linspace(0, len(vtr)*step_tempo, len(vtr))

        d = {'Tempo': tempo_ptr, 'Ptr': ptr, 'Vtr': vtr, 'AN': action_number_ptr}
        df_ptr = pd.DataFrame(data=d)

        print(df_ptr)

        action_numbers = pd.unique(df_ptr.AN)

        for action_number in action_numbers:
            print(action_number)
            df_ptr2 = df_ptr[df_ptr['AN'] == action_number]
            texto = '{an} - {et}'.format(an=action_number, et=lista_dosagens[action_number])
            self.MplWidget2.canvas2.axes1.plot(df_ptr2.Tempo, df_ptr2.Ptr, linewidth=0.5, label=texto)
            self.MplWidget2.canvas2.axes2.plot(df_ptr2.Tempo, df_ptr2.Vtr, linewidth=0.5, label=texto)
            self.cbEtapas_4.addItem(texto)
        self.MplWidget2.canvas2.axes1.legend(loc='best', prop={'size': 6})
        self.MplWidget2.canvas2.axes1.set_title('Sinal Completo Pressão Traqueal', fontsize=10)
        self.MplWidget2.canvas2.axes1.set_ylabel('Ptr (cmH2O)')

        self.MplWidget2.canvas2.axes2.legend(loc='best', prop={'size': 6})
        self.MplWidget2.canvas2.axes2.set_title('Sinal Completo Volume Traqueal', fontsize=10)
        self.MplWidget2.canvas2.axes2.set_ylabel('Vtr (mL)')
        self.MplWidget2.canvas2.draw()

    def carrega_dados_ptr_2(self):
        global df, df_ptr, lista_dosagens
        df = []
        df_ptr = []

        arquivo_dados_brutos_ptr_2 = self.txArquivoBruto_6.text()
        tx_animal_ptr_2 = self.txAnimal_6.text()

        if arquivo_dados_brutos_ptr_2 == '' or tx_animal_ptr_2 == '':
            self.show_popup('Preencha os campos!')
            return

        padding = 0

        lista_dosagens = ['PBS', '3 ug/kg', '10 ug/kg', '30 ug/kg', '100 ug/kg', '300 ug/kg', 'N/A']
        try:
            rawdata = open(arquivo_dados_brutos_ptr_2, 'rb').read()
            res = chardet.detect(rawdata)
        except FileNotFoundError:
            self.show_popup('Arquivo Ptr não encontrado!')
            return

        if res['encoding'] == 'UTF-16':  # flexiVent novo
            self.label_8.setText('flexiWare')
            # print('flexiVent novo')
            lista_skip = list(range(0, 13)) + [14]
            df = pd.read_csv(arquivo_dados_brutos_ptr_2, sep='\\t', header=0, error_bad_lines=False,
                             encoding='utf-16', skiprows=lista_skip, engine='python')
        else:  # flexiVent Antigo
            self.label_8.setText('Legacy')
            padding = 1
            # print('flexiVent antigo')
            lista_skip = list(range(0, 21)) + [22]
            df = pd.read_csv(arquivo_dados_brutos_ptr_2, sep='\\t', header=0, error_bad_lines=False,
                             encoding='utf-8', skiprows=lista_skip, engine='python')

        print(df)
        self.MplWidget4.canvas4.axes1.clear()
        self.MplWidget4.canvas4.axes2.clear()
        # self.MplWidget4.canvas4.axes3.clear()
        # self.MplWidget4.canvas4.axes4.clear()
        self.cbEtapas_7.clear()

        coluna_inicio = 0
        contador_coluna = 0

        diff_s = np.diff(df.Time)
        step_tempo = np.mean(diff_s)

        for column_name in df.columns:  # Verifica quando dosagens começam
            if 'Ptr' in column_name:
                # print(column_name)
                if pd.Series.value_counts(df[column_name]).__len__() > 780:
                    coluna_inicio = contador_coluna + 1
            contador_coluna += 1

        print(coluna_inicio)

        ptr = []
        vtr = []
        action_number_ptr = []
        action_number_vtr = []
        contador_perturbacoes_ptr = 0
        contador_perturbacoes_vtr = 0
        perturbacao_ptr = 0
        perturbacao_vtr = 0
        for column_name in df.columns[coluna_inicio:]:
            if 'Ptr' in column_name:
                ptr_limpa = df[column_name]  # .dropna()
                ptr.extend(ptr_limpa)  # .values)
                if padding:
                    ptr.extend([np.nan] * 769)
                action_number_ptr.extend([perturbacao_ptr] * (len(ptr_limpa) + (padding * 769)))
                if contador_perturbacoes_ptr != 0 and contador_perturbacoes_ptr % 14 == 0:
                    perturbacao_ptr += 1
                    contador_perturbacoes_ptr = 0
                else:
                    contador_perturbacoes_ptr += 1

            if 'Vtr' in column_name:
                vtr_limpa = df[column_name]  # .dropna()
                vtr.extend(vtr_limpa)  # .values)
                if padding:
                    vtr.extend([np.nan] * 769)
                action_number_vtr.extend([perturbacao_vtr] * (len(vtr_limpa) + (padding * 769)))
                if contador_perturbacoes_vtr != 0 and contador_perturbacoes_vtr % 14 == 0:
                    perturbacao_vtr += 1
                    contador_perturbacoes_vtr = 0
                else:
                    contador_perturbacoes_vtr += 1

        tempo_ptr = np.linspace(0, len(ptr)*step_tempo, len(ptr))
        # tempo_vtr = np.linspace(0, len(vtr)*step_tempo, len(vtr))

        d = {'Tempo': tempo_ptr, 'Ptr': ptr, 'Vtr': vtr, 'AN': action_number_ptr}
        df_ptr = pd.DataFrame(data=d)

        print(df_ptr)

        action_numbers = pd.unique(df_ptr.AN)

        for action_number in action_numbers:
            print(action_number)
            df_ptr2 = df_ptr[df_ptr['AN'] == action_number]
            texto = '{an} - {et}'.format(an=action_number, et=lista_dosagens[action_number])
            self.MplWidget4.canvas4.axes1.plot(df_ptr2.Tempo, df_ptr2.Ptr, linewidth=0.5, label=texto)
            self.MplWidget4.canvas4.axes2.plot(df_ptr2.Tempo, df_ptr2.Vtr, linewidth=0.5, label=texto)
            self.cbEtapas_7.addItem(texto)

        self.MplWidget4.canvas4.axes1.legend(loc='best', prop={'size': 6})
        self.MplWidget4.canvas4.axes1.set_title('Sinal Completo Pressão Traqueal', fontsize=10)
        self.MplWidget4.canvas4.axes1.set_ylabel('Ptr (cmH2O)')

        self.MplWidget4.canvas4.axes2.legend(loc='best', prop={'size': 6})
        self.MplWidget4.canvas4.axes2.set_title('Sinal Completo Volume Traqueal', fontsize=10)
        self.MplWidget4.canvas4.axes2.set_ylabel('Vtr (mL)')
        self.MplWidget4.canvas4.draw()

    def busca_arquivo_dados_brutos_pa(self):


        print('PA')
        arquivo_dados_brutos_pa, _ = QFileDialog.getOpenFileName(self, "QFileDialog.getOpenFileName()", "",
                                                                 "Text Files (*.txt);; Log Files (*.log)")

        if arquivo_dados_brutos_pa:
            print(arquivo_dados_brutos_pa)
            self.txArquivoBruto_3.setText(arquivo_dados_brutos_pa)
        else:
            print('nao abriu')

    def busca_arquivo_dados_brutos_pa_2(self):


        print('PA')
        arquivo_dados_brutos_pa_2, _ = QFileDialog.getOpenFileName(self, "QFileDialog.getOpenFileName()", "",
                                                                   "Text Files (*.txt);; Log Files (*.log)")

        if arquivo_dados_brutos_pa_2:
            print(arquivo_dados_brutos_pa_2)
            self.txArquivoBruto_5.setText(arquivo_dados_brutos_pa_2)
        else:
            print('nao abriu')

    def busca_arquivo_dados_brutos_ptr(self):

        print('Ptr')
        arquivo_dados_brutos_ptr, _ = QFileDialog.getOpenFileName(self, "QFileDialog.getOpenFileName()", "",
                                                                  "Text Files (*.txt);; Excel Files (*.xlsx)")

        if arquivo_dados_brutos_ptr:
            print(arquivo_dados_brutos_ptr)
            self.txArquivoBruto_4.setText(arquivo_dados_brutos_ptr)
        else:
            print('nao abriu')

    def busca_arquivo_dados_brutos_ptr_2(self):

        print('Ptr')
        arquivo_dados_brutos_ptr_2, _ = QFileDialog.getOpenFileName(self, "QFileDialog.getOpenFileName()", "",
                                                                    "Text Files (*.txt);; Excel Files (*.xlsx)")

        if arquivo_dados_brutos_ptr_2:
            print(arquivo_dados_brutos_ptr_2)
            self.txArquivoBruto_6.setText(arquivo_dados_brutos_ptr_2)
        else:
            print('nao abriu')

    def busca_arquivo_parametros(self):
        print('PA')
        arquivo_parametros, _ = QFileDialog.getOpenFileName(self, "QFileDialog.getOpenFileName()", "",
                                                            "Text Files (*.ods)")
        if arquivo_parametros:
            print(arquivo_parametros)
            self.txParametros_3.setText(arquivo_parametros)
        else:
            print('nao abriu')

    def busca_arquivo_parametros_2(self):
        print('PA')
        arquivo_parametros_2, _ = QFileDialog.getOpenFileName(self, "QFileDialog.getOpenFileName()", "",
                                                              "Text Files (*.ods)")
        if arquivo_parametros_2:
            print(arquivo_parametros_2)
            self.txParametros_4.setText(arquivo_parametros_2)
        else:
            print('nao abriu')

    def busca_arquivo_saida(self):

        arquivo_saida, _ = QFileDialog.getOpenFileName(self, "QFileDialog.getOpenFileName()", "",
                                                       "CSV Files (*.csv)")
        if arquivo_saida:
            print(arquivo_saida)
            self.txArquivoSaida_3.setText(arquivo_saida)
            self.txArquivoSaida_4.setText(arquivo_saida)
        else:
            print('nao abriu')

    def limpa_campos_pa(self):
        self.txArquivoBruto_3.setText('')
        self.txParametros_3.setText('')
        self.txAnimal_3.setText('')
        self.txArquivoSaida_3.setText('')

    def limpa_graficos_pa(self):
        self.MplWidget3.canvas3.axes1.clear()
        self.MplWidget3.canvas3.axes1.draw()
        self.MplWidget3.canvas3.axes2.clear()
        self.MplWidget3.canvas3.axes2.draw()
        self.cbEtapas_5.clear()

    def limpa_campos_pa_2(self):
        self.txArquivoBruto_5.setText('')
        self.txParametros_4.setText('')
        self.txAnimal_5.setText('')

    def limpa_campos_ptr(self):
        self.txArquivoBruto_4.setText('')
        self.txAnimal_4.setText('')
        self.txArquivoSaida_4.setText('')

    def limpa_campos_ptr_2(self):
        self.txArquivoBruto_6.setText('')
        self.txAnimal_6.setText('')

    def limpa_graficos_ptr(self):
        self.MplWidget4.canvas4.axes1.clear()
        self.MplWidget4.canvas4.axes2.clear()
        self.MplWidget4.canvas4.axes3.clear()
        self.MplWidget4.canvas4.axes4.clear()
        self.MplWidget4.canvas4.axes1.draw()
        self.MplWidget4.canvas4.axes2.draw()
        self.MplWidget4.canvas4.axes3.draw()
        self.MplWidget4.canvas4.axes4.draw()
        self.cbEtapas_7.clear()

    def atualiza_dados_pa(self):
        global df_tukey
        df_tukey = []

        self.MplWidget_3.canvas.axes2.clear()
        action_number = self.cbEtapas_3.currentIndex()

        df3 = df[df['AN'] == action_number]

        df_tukey = df3

        if self.rbRice_3.isChecked():
            pa_filtrada = []
            pa_trecho = list(df_tukey['PA'].values)
            janela = int(np.floor((self.spinBox.value()/100)*2*(len(pa_trecho)**(1/3))))
            print(janela, pa_filtrada)
            for i in range(0, len(pa_trecho), janela):
                pa_janela = pa_trecho[i:i+janela]
                lim_sup, lim_inf = self.define_quartis(pa_janela)
                if lim_inf > 5 and lim_sup < 200:
                    # melhoria: pegar media tirando outliers
                    media_trecho_aux = [x for x in pa_janela if lim_sup > x > lim_inf]
                    media_trecho = np.mean(media_trecho_aux)
                    pa_janela_aux = [media_trecho if pa > lim_sup or pa < lim_inf else pa for pa in pa_janela]
                    pa_janela = pa_janela_aux
                elif lim_inf < 5:
                    media_trecho_aux = [x for x in pa_janela if lim_sup > x > lim_inf]
                    media_trecho = np.mean(media_trecho_aux)
                    pa_janela_aux = [media_trecho if pa > lim_sup or pa < lim_inf else pa for pa in pa_janela]
                    pa_janela = pa_janela_aux
                elif lim_sup > 200:
                    media_trecho_aux = [x for x in pa_janela if lim_sup > x > lim_inf]
                    media_trecho = np.mean(media_trecho_aux)
                    pa_janela_aux = [media_trecho if pa > lim_sup or pa < lim_inf else pa for pa in pa_janela]
                    pa_janela = pa_janela_aux
                else:
                    media_trecho_aux = [x for x in pa_janela if lim_sup > x > lim_inf]
                    media_trecho = np.mean(media_trecho_aux)
                    pa_janela_aux = [media_trecho if pa > lim_sup or pa < lim_inf else pa for pa in pa_janela]
                    pa_janela = pa_janela_aux

                pa_filtrada.extend(pa_janela)
                # print(pa_filtrada)

            df_tukey['PA'] = pa_filtrada

        elif self.rbEtapa_3.isChecked():
            lim_sup, lim_inf = self.define_quartis(df3.PA.values)

            valores_pa = df3.PA.values
            media_trecho_aux = [x for x in valores_pa if lim_sup > x > lim_inf]
            media_trecho = np.mean(media_trecho_aux)

            df_tukey['PA'].mask(df_tukey['PA'] > lim_sup, media_trecho, inplace=True)
            df_tukey['PA'].mask(df_tukey['PA'] < lim_inf, media_trecho, inplace=True)
        else:
            print('erro janela')

        texto = '{an} - {et}'.format(an=action_number, et=etapas.Oc[action_number])
        self.MplWidget_3.canvas.axes2.plot(df_tukey.Time, df_tukey.PA, linewidth=0.5, label=texto)
        self.MplWidget_3.canvas.axes2.legend(loc='best', prop={'size': 6})
        self.MplWidget_3.canvas.axes2.set_xlabel('Tempo (s)')
        self.MplWidget_3.canvas.axes2.set_title('Trecho Pressão Arterial', fontsize=10)
        self.MplWidget_3.canvas.axes2.set_ylabel('PA (mmHg)')
        self.MplWidget_3.canvas.draw()

    def atualiza_dados_pa_2(self):
        global df_tukey
        df_tukey = []

        # self.MplWidget3.canvas3.axes2.clear()
        action_number = self.cbEtapas_5.currentIndex()

        df3 = df[df['AN'] == action_number]

        df_tukey = df3

        if self.rbRice_4.isChecked():
            pa_filtrada = []
            pa_trecho = list(df_tukey['PA'].values)
            janela = int(np.floor((self.spinBox.value()/100)*2*(len(pa_trecho)**(1/3))))
            print(janela, pa_filtrada)
            for i in range(0, len(pa_trecho), janela):
                pa_janela = pa_trecho[i:i+janela]
                lim_sup, lim_inf = self.define_quartis(pa_janela)
                if lim_inf > 5 and lim_sup < 200:
                    # melhoria: pegar media tirando outliers
                    media_trecho_aux = [x for x in pa_janela if lim_sup > x > lim_inf]
                    media_trecho = np.mean(media_trecho_aux)
                    pa_janela_aux = [media_trecho if pa > lim_sup or pa < lim_inf else pa for pa in pa_janela]
                    pa_janela = pa_janela_aux
                elif lim_inf < 5:
                    media_trecho_aux = [x for x in pa_janela if lim_sup > x > lim_inf]
                    media_trecho = np.mean(media_trecho_aux)
                    pa_janela_aux = [media_trecho if pa > lim_sup or pa < lim_inf else pa for pa in pa_janela]
                    pa_janela = pa_janela_aux
                elif lim_sup > 200:
                    media_trecho_aux = [x for x in pa_janela if lim_sup > x > lim_inf]
                    media_trecho = np.mean(media_trecho_aux)
                    pa_janela_aux = [media_trecho if pa > lim_sup or pa < lim_inf else pa for pa in pa_janela]
                    pa_janela = pa_janela_aux
                else:
                    media_trecho_aux = [x for x in pa_janela if lim_sup > x > lim_inf]
                    media_trecho = np.mean(media_trecho_aux)
                    pa_janela_aux = [media_trecho if pa > lim_sup or pa < lim_inf else pa for pa in pa_janela]
                    pa_janela = pa_janela_aux

                pa_filtrada.extend(pa_janela)
                # print(pa_filtrada)

            df_tukey['PA'] = pa_filtrada

        elif self.rbEtapa_4.isChecked():
            lim_sup, lim_inf = self.define_quartis(df3.PA.values)

            valores_pa = df3.PA.values
            media_trecho_aux = [x for x in valores_pa if lim_sup > x > lim_inf]
            media_trecho = np.mean(media_trecho_aux)

            df_tukey['PA'].mask(df_tukey['PA'] > lim_sup, media_trecho, inplace=True)
            df_tukey['PA'].mask(df_tukey['PA'] < lim_inf, media_trecho, inplace=True)
        else:
            print('erro janela')

        # texto = '{an} - {et}'.format(an=action_number, et=etapas.Oc[action_number])
        texto = '{gr} - Animal {an} - {et}'.format(gr=self.cbGrupoAnimais_5.currentText(), an=self.txAnimal_5.text(),
                                                   et=etapas.Oc[action_number])
        self.MplWidget3.canvas3.axes2.plot(df_tukey.Time, df_tukey.PA, linewidth=0.5, label=texto)
        self.MplWidget3.canvas3.axes2.legend(loc='best', prop={'size': 6})
        self.MplWidget3.canvas3.axes2.set_xlabel('Tempo (s)')
        self.MplWidget3.canvas3.axes2.set_title('Trecho Pressão Arterial', fontsize=10)
        self.MplWidget3.canvas3.axes2.set_ylabel('PA (mmHg)')
        self.MplWidget3.canvas3.draw()

    def atualiza_dados_ptr(self):
        global df_ptr3, medias_ptr, desvios_ptr

        medias_ptr = []
        desvios_ptr = []

        df_ptr3 = []

        self.MplWidget2.canvas2.axes3.clear()
        self.MplWidget2.canvas2.axes4.clear()
        action_number = self.cbEtapas_4.currentIndex()

        df_ptr3 = df_ptr[df_ptr['AN'] == action_number]

        texto = '{an} - {et}'.format(an=action_number, et=lista_dosagens[action_number])
        self.MplWidget2.canvas2.axes3.plot(df_ptr3.Tempo, df_ptr3.Ptr, linewidth=0.5, label=texto)
        self.MplWidget2.canvas2.axes3.legend(loc='best', prop={'size': 6})
        self.MplWidget2.canvas2.axes3.set_xlabel('Tempo (s)')
        self.MplWidget2.canvas2.axes3.set_title('Trecho Pressão Traqueal', fontsize=10)
        self.MplWidget2.canvas2.axes3.set_ylabel('Ptr (cmH2O)')

        self.MplWidget2.canvas2.axes4.plot(df_ptr3.Tempo, df_ptr3.Vtr, linewidth=0.5, label=texto)
        self.MplWidget2.canvas2.axes4.legend(loc='best', prop={'size': 6})
        self.MplWidget2.canvas2.axes4.set_xlabel('Tempo (s)')
        self.MplWidget2.canvas2.axes4.set_title('Trecho Volume Traqueal', fontsize=10)
        self.MplWidget2.canvas2.axes4.set_ylabel('Vtr (mL)')
        self.MplWidget2.canvas2.draw()

        df3_ptr_limpa = df_ptr3['Ptr'].dropna()
        df3_lista = df3_ptr_limpa.to_list()
        passo = int(len(df3_lista) / 15)
        print(passo)

        for a in range(0, len(df3_lista), passo):
            medias_ptr.append(np.mean(df3_lista[a:a+passo]))
            desvios_ptr.append(np.std(df3_lista[a:a+passo]))

    def atualiza_dados_ptr_2(self):
        global df_ptr3, medias_ptr, desvios_ptr

        medias_ptr = []
        desvios_ptr = []

        df_ptr3 = []

        # self.MplWidget4.canvas4.axes3.clear()
        # self.MplWidget4.canvas4.axes4.clear()
        action_number = self.cbEtapas_7.currentIndex()

        df_ptr3 = df_ptr[df_ptr['AN'] == action_number]

        # texto = '{an} - {et}'.format(an=action_number, et=lista_dosagens[action_number])
        texto = '{gr} - Animal {an} - {et}'.format(gr=self.cbGrupoAnimais_6.currentText(), an=self.txAnimal_6.text(),
                                            et=etapas.Oc[action_number])
        self.MplWidget4.canvas4.axes3.plot(df_ptr3.Tempo, df_ptr3.Ptr, linewidth=0.5, label=texto)
        self.MplWidget4.canvas4.axes3.legend(loc='best', prop={'size': 6})
        self.MplWidget4.canvas4.axes3.set_xlabel('Tempo (s)')
        self.MplWidget4.canvas4.axes3.set_title('Trecho Pressão Traqueal', fontsize=10)
        self.MplWidget4.canvas4.axes3.set_ylabel('Ptr (cmH2O)')

        self.MplWidget4.canvas4.axes4.plot(df_ptr3.Tempo, df_ptr3.Vtr, linewidth=0.5, label=texto)
        self.MplWidget4.canvas4.axes4.legend(loc='best', prop={'size': 6})
        self.MplWidget4.canvas4.axes4.set_xlabel('Tempo (s)')
        self.MplWidget4.canvas4.axes4.set_title('Trecho Volume Traqueal', fontsize=10)
        self.MplWidget4.canvas4.axes4.set_ylabel('Vtr (mL)')
        self.MplWidget4.canvas4.draw()

        df3_ptr_limpa = df_ptr3['Ptr'].dropna()
        df3_lista = df3_ptr_limpa.to_list()
        passo = int(len(df3_lista) / 15)
        print(passo)

        for a in range(0, len(df3_lista), passo):
            medias_ptr.append(np.mean(df3_lista[a:a+passo]))
            desvios_ptr.append(np.std(df3_lista[a:a+passo]))

    def define_quartis(self, trecho_sinal):
        # Q2 = np.quantile(trecho_sinal, 0.5)
        q1 = np.quantile(trecho_sinal, 0.25)
        q3 = np.quantile(trecho_sinal, 0.75)
        iqr = q3 - q1

        if self.rb15IQR_3.isChecked():
            lim_inf_ = q1 - 1.5 * iqr
            lim_sup_ = q3 + 1.5 * iqr
        elif self.rb3IQR_3.isChecked():
            lim_inf_ = q1 - 3 * iqr
            lim_sup_ = q3 + 3 * iqr
        elif self.rbNenhum_3.isChecked():
            lim_inf_ = min(trecho_sinal)
            lim_sup_ = max(trecho_sinal)
        else:
            print('erro outliers')

        return lim_sup_, lim_inf_

    def identifica_perturbacoes(self, df_pa, tempo):
        print(df_pa)
        print(tempo)
        inicio_intra = []
        inicio_inter = []
        fim_intra = []
        fim_inter = []
        if self.checkBoxTempo.isChecked():
            ajuste_wky = 1.3333333333333333
        else:
            ajuste_wky = 1

        for i in range(0, 15):
            if tempo + 9 < df_pa['Time'].iloc[-1]:
                intra = df_pa[(df_pa['Time'] > tempo + 1.5/ajuste_wky) & (df_pa['Time'] < tempo + 3.5/ajuste_wky)]
                inter = df_pa[(df_pa['Time'] > tempo + 7/ajuste_wky) & (df_pa['Time'] < tempo + 9/ajuste_wky)]
                intra_aux = intra['Time'].index.tolist()
                inter_aux = inter['Time'].index.tolist()
                print(intra_aux)
                print(inter_aux)
                inicio_intra.append(intra_aux[0])
                fim_intra.append(intra_aux[-1])
                inicio_inter.append(inter_aux[0])
                fim_inter.append(inter_aux[-1])
                tempo += 11/ajuste_wky
            else:
                print('sinal pequeno')
                self.show_popup('Sinal Pequeno \n Considere selecionar o Ajuste de Tempo')
                inicio_intra, fim_intra, inicio_inter, fim_inter = None, None, None, None
                break

        return inicio_intra, fim_intra, inicio_inter, fim_inter

    def detecta_inicio_pa(self):
        global cid
        self.atualiza_dados_pa()
        cid = self.MplWidget_3.canvas.mpl_connect('button_release_event', self.onclick)

    def calcula_dados_perturbacoes(self, inicios, fins):
        medias_trechos = []
        desvios_trechos = []
        for inicio, fim in zip(inicios, fins):
            medias_trechos.append(np.mean(df_tukey.PA.loc[inicio:fim]))
            desvios_trechos.append(np.std(df_tukey.PA.loc[inicio:fim]))

        return medias_trechos, desvios_trechos

    def show_popup(self, txt_msgbox):
        msg = QMessageBox()
        msg.setWindowTitle('Atenção!')
        msg.setIcon(QMessageBox.Warning)
        msg.setText(txt_msgbox)
        x = msg.exec_()

    def show_popup_sobre(self):
        msg_sobre = QMessageBox()
        msg_sobre.setWindowTitle('Sobre o software')
        msg_sobre.setIcon(QMessageBox.Information)
        msg_sobre.setText('Software desenvolvido como parte da dissertação de \n Mestrado '
                          'de Leandro Consentino Ferreira \n\n Contato: leandroconsentinoferreira@gmail.com')
        x_sobre = msg_sobre.exec_()


if __name__ == "__main__":
    # app = QApplication(sys.argv)
    app = QApplication([])
    win = Window()
    win.show()
    sys.exit(app.exec_())
