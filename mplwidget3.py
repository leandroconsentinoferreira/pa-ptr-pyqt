# ------------------------------------------------------
# -------------------- mplwidget.py --------------------
# ------------------------------------------------------
from PySide2.QtWidgets import *

from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg
from matplotlib.figure import Figure
from matplotlib.backends.backend_qt5agg import (NavigationToolbar2QT as NavigationToolbar)


class MplWidget3(QWidget):

    def __init__(self, parent=None):
        QWidget.__init__(self, parent)

        self.canvas3 = FigureCanvasQTAgg(Figure(tight_layout=True))

        vertical_layout3 = QVBoxLayout()
        vertical_layout3.addWidget(self.canvas3)
        self.toolbar3 = NavigationToolbar(self.canvas3, self)
        vertical_layout3.addWidget(self.toolbar3)

        self.canvas3.axes1 = self.canvas3.figure.add_subplot(211)
        self.canvas3.axes2 = self.canvas3.figure.add_subplot(212)
        self.setLayout(vertical_layout3)
