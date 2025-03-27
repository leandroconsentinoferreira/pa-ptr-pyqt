# ------------------------------------------------------
# -------------------- mplwidget.py --------------------
# ------------------------------------------------------
from PySide2.QtWidgets import *

from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg
from matplotlib.figure import Figure
from matplotlib.backends.backend_qt5agg import (NavigationToolbar2QT as NavigationToolbar)


class MplWidget2(QWidget):

    def __init__(self, parent=None):
        QWidget.__init__(self, parent)

        self.canvas2 = FigureCanvasQTAgg(Figure(tight_layout=True))

        vertical_layout2 = QVBoxLayout()
        vertical_layout2.addWidget(self.canvas2)
        self.toolbar2 = NavigationToolbar(self.canvas2, self)
        vertical_layout2.addWidget(self.toolbar2)

        self.canvas2.axes1 = self.canvas2.figure.add_subplot(221)
        self.canvas2.axes2 = self.canvas2.figure.add_subplot(222)
        self.canvas2.axes3 = self.canvas2.figure.add_subplot(223)
        self.canvas2.axes4 = self.canvas2.figure.add_subplot(224)
        self.setLayout(vertical_layout2)
