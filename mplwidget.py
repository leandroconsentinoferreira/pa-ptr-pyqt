# ------------------------------------------------------
# -------------------- mplwidget.py --------------------
# ------------------------------------------------------
from PySide2.QtWidgets import *

from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg
from matplotlib.figure import Figure
from matplotlib.backends.backend_qt5agg import (NavigationToolbar2QT as NavigationToolbar)


class MplWidget(QWidget):

    def __init__(self, parent=None):
        QWidget.__init__(self, parent)

        self.canvas = FigureCanvasQTAgg(Figure(tight_layout=True))

        vertical_layout = QVBoxLayout()
        vertical_layout.addWidget(self.canvas)
        self.toolbar = NavigationToolbar(self.canvas, self)
        vertical_layout.addWidget(self.toolbar)

        self.canvas.axes1 = self.canvas.figure.add_subplot(211)
        self.canvas.axes2 = self.canvas.figure.add_subplot(212)
        self.setLayout(vertical_layout)
