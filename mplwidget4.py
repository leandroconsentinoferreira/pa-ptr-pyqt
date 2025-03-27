# ------------------------------------------------------
# -------------------- mplwidget.py --------------------
# ------------------------------------------------------
from PySide2.QtWidgets import *

from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg
from matplotlib.figure import Figure
from matplotlib.backends.backend_qt5agg import (NavigationToolbar2QT as NavigationToolbar)


class MplWidget4(QWidget):

    def __init__(self, parent=None):
        QWidget.__init__(self, parent)

        self.canvas4 = FigureCanvasQTAgg(Figure(tight_layout=True))

        vertical_layout4 = QVBoxLayout()
        vertical_layout4.addWidget(self.canvas4)
        self.toolbar4 = NavigationToolbar(self.canvas4, self)
        vertical_layout4.addWidget(self.toolbar4)

        self.canvas4.axes1 = self.canvas4.figure.add_subplot(221)
        self.canvas4.axes2 = self.canvas4.figure.add_subplot(222)
        self.canvas4.axes3 = self.canvas4.figure.add_subplot(223)
        self.canvas4.axes4 = self.canvas4.figure.add_subplot(224)
        self.setLayout(vertical_layout4)
