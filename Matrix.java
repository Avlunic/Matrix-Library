public class Matrix {

	int rows;
	int cols;
	double[][] matrix;

	public Matrix(double[][] matrix) {
		if (matrix.length > 0 && matrix[0].length > 0) {
			this.matrix = matrix;
			this.rows = matrix.length;
			this.cols = matrix[0].length;
		} else {
			System.out.println("Matrix size must be greater than 0");
		}
	}

	public Matrix(Matrix matrix) {
		this.matrix = matrix.getData();
		this.rows = matrix.getRows();
		this.cols = matrix.getColumns();
	}

	public Matrix(int rows, int cols) {
		if (rows > 0 && cols > 0) {
			this.matrix = new double[rows][cols];
			this.rows = rows;
			this.cols = cols;
			for (int i = 0; i < rows; i++) {
				for (int j = 0; j < cols; j++) {
					this.matrix[i][j] = 0;
				}
			}
		} else {
			System.out.println("Matrix size must be greater than 0");
		}
	}

	public void multiply(Matrix matrix) {
		int rows = this.getRows();
		int cols = matrix.getColumns();
		double[] colsArray = new double[matrix.getRows()];
		double[] rowsArray = new double[this.cols];
		for (int i = 0; i < matrix.getRows(); i++) {
			colsArray[i] = 0;
			rowsArray[i] = 0;
		}
		double[][] tempMatrix = new double[rows][cols];
		if (this.cols == matrix.getRows()) {
			for (int i = 0; i < rows; i++) {
				for (int j = 0; j < cols; j++) {
					for (int x = 0; x < matrix.getRows(); x++) {
						rowsArray[x] = this.matrix[i][x];
						colsArray[x] = matrix.getValueAt(x, j);
					}
					tempMatrix[i][j] = dotRows(rowsArray, colsArray);
				}
			}
			this.rows = rows;
			this.cols = cols;
			this.matrix = tempMatrix;
		} else {
			System.out.println("Check matrix dimensions");
		}
	}

	public void rowEchelonForm() {
		int currentRow = -1;
		int tempRow = currentRow;
		double multiplyValue = 0;
		for (int i = 0; i < this.cols; i++) {
			for (int j = currentRow + 1; j < this.rows; j++) {
				if (this.matrix[j][i] != 0) {
					tempRow = currentRow;
					currentRow = j;
					if (j != currentRow + 1) {
						this.swapRows(tempRow + 1, j);
						currentRow = tempRow + 1;
					}
					break;
				}
			}
			for (int a = currentRow + 1; a < this.rows; a++) {
				if (this.matrix[a][i] != 0) {
					multiplyValue = this.matrix[a][i] / this.matrix[currentRow][i];
					for (int b = 0; b < this.cols; b++) {
						this.matrix[a][b] -= multiplyValue * this.matrix[currentRow][b];
						if (i == b) {
							this.matrix[a][b] = 0;
						}
					}
				}
			}
		}
	}

	public void rowReducedEchelonForm() {
		this.rowEchelonForm();
		this.colZero();
		double value = 0;
		for (int i = 1; i < this.rows; i++) {
			for (int j = 0; j < this.cols; j++) {
				if (this.getValueAt(i, j) == 1) {
					for (int b = 0; b < i; b++) {
						value = this.getValueAt(b, j) / this.getValueAt(i, j);
						for (int a = 0; a < this.cols; a++) {
							this.matrix[b][a] -= value * this.getValueAt(i, a);
							if (j == a) {
								this.matrix[b][a] = 0;
							}
						}
					}
					break;
				}
			}
		}
	}

	public double determinant() {
		if (this.rows == this.cols) {
			double determinant = 0;
			if (this.rows == 2 && this.cols == 2) {
				determinant = this.getValueAt(0, 0) * this.getValueAt(1, 1)
						- this.getValueAt(0, 1) * this.getValueAt(1, 0);
			} else {
				double[][] m = new double[this.rows - 1][this.cols - 1];
				int place = 0;
				for (int i = 0; i < this.rows; i++) {
					for (int a = 1; a < this.rows; a++) {
						for (int b = 0; b < this.cols; b++) {
							if (b < place) {
								m[a - 1][b] = this.getValueAt(a, b);
							} else if (b > place) {
								m[a - 1][b - 1] = this.getValueAt(a, b);
							}
						}
					}
					if (place % 2 == 0) {
						determinant += this.getValueAt(0, place) * determinant(new Matrix(m));
					} else {
						determinant -= this.getValueAt(0, place) * determinant(new Matrix(m));
					}
					place++;
				}
			}
			return determinant;
		} else {
			System.out.println("Matrix dimensions must match");
			return 0;
		}
	}

	private double determinant(Matrix matrix) {
		double determinant = 0;
		if (matrix.getRows() == 2 && matrix.getColumns() == 2) {
			determinant = matrix.getValueAt(0, 0) * matrix.getValueAt(1, 1)
					- matrix.getValueAt(0, 1) * matrix.getValueAt(1, 0);
		} else {
			double[][] m = new double[matrix.getRows() - 1][matrix.getColumns() - 1];
			int place = 0;
			for (int i = 0; i < matrix.getRows(); i++) {
				for (int a = 1; a < matrix.getRows(); a++) {
					for (int b = 0; b < matrix.getColumns(); b++) {
						if (b < place) {
							m[a - 1][b] = matrix.getValueAt(a, b);
						} else if (b > place) {
							m[a - 1][b - 1] = matrix.getValueAt(a, b);
						}
					}
				}
				if (place % 2 == 0) {
					determinant += matrix.getValueAt(0, place) * determinant(new Matrix(m));
				} else {
					determinant -= matrix.getValueAt(0, place) * determinant(new Matrix(m));
				}
				place++;
			}
		}
		return determinant;
	}

	public void inverse() {
		this.addIdentityMatrix();
		this.print();
		this.rowReducedEchelonForm();
		this.removeIdentityMatrix();
	}

	public void transpose() {
		double[][] m = new double[this.cols][this.rows];
		for (int i = 0; i < this.cols; i++) {
			for (int j = 0; j < this.rows; j++) {
				m[i][j] = this.getValueAt(j, i);
			}
		}
		Matrix n = new Matrix(m);
		this.matrix = n.getData();
		this.rows = n.getRows();
		this.cols = n.getColumns();
	}

	public void add(Matrix matrix) {
		int rows = matrix.getRows();
		int cols = matrix.getColumns();
		if (this.rows == rows && this.cols == cols) {
			for (int i = 0; i < this.rows; i++) {
				for (int j = 0; j < this.cols; j++) {
					this.matrix[i][j] += matrix.getValueAt(i, j);
				}
			}
		} else {
			System.out.println("Matrix dimensions must match");
		}
	}

	public void subtract(Matrix matrix) {
		int rows = matrix.getRows();
		int cols = matrix.getColumns();
		if (this.rows == rows && this.cols == cols) {
			for (int i = 0; i < this.rows; i++) {
				for (int j = 0; j < this.cols; j++) {
					this.matrix[i][j] -= matrix.getValueAt(i, j);
				}
			}
		} else {
			System.out.println("Matrix dimensions must match");
		}
	}

	public void swapRows(int row1, int row2) {
		double temp = 0;
		for (int i = 0; i < this.cols; i++) {
			temp = this.matrix[row1][i];
			this.matrix[row1][i] = this.matrix[row2][i];
			this.matrix[row2][i] = temp;
		}
	}

	private double dotRows(double[] rowsArray, double[] colsArray) {
		double sum = 0;
		for (int i = 0; i < colsArray.length; i++) {
			sum += (colsArray[i] * rowsArray[i]);
		}
		return sum;
	}

	private void colZero() {
		double value = 0;
		for (int i = 0; i < this.rows; i++) {
			for (int j = 0; j < this.cols; j++) {
				if (this.getValueAt(i, j) != 0) {
					value = 1 / this.getValueAt(i, j);
					for (int a = 0; a < this.cols; a++) {
						this.matrix[i][a] *= value;
					}
					this.matrix[i][j] = 1;
					break;
				}
			}
		}
	}

	private void addIdentityMatrix() {
		if (this.rows == this.cols) {
			double[][] m = new double[this.rows][this.cols * 2];
			for (int i = 0; i < this.rows; i++) {
				for (int j = 0; j < this.cols; j++) {
					m[i][j] = this.getValueAt(i, j);
				}
			}
			for (int i = 0; i < this.rows; i++) {
				for (int j = 0; j < this.cols; j++) {
					if (i != j) {
						m[i][j + this.cols] = 0;
					} else {
						m[i][j + this.cols] = 1;
					}
				}
			}
			Matrix n = new Matrix(m);
			this.matrix = n.getData();
			this.rows = n.getRows();
			this.cols = n.getColumns();
		} else {
			System.out.println("Matrix dimensions must match");
		}
	}

	private void removeIdentityMatrix() {
		double[][] m = new double[this.rows][this.cols / 2];
		for (int i = 0; i < this.rows; i++) {
			for (int j = 0; j < this.cols / 2; j++) {
				System.out.println(j);
				m[i][j] = this.getValueAt(i, j + this.cols / 2);
			}
		}
		Matrix n = new Matrix(m);
		this.matrix = n.getData();
		this.rows = n.getRows();
		this.cols = n.getColumns();
	}

	public void setValueAt(double value, int rows, int cols) {
		this.matrix[rows][cols] = value;
	}

	public double getValueAt(int rows, int cols) {
		return this.matrix[rows][cols];
	}

	public int getRows() {
		return this.matrix.length;
	}

	public int getColumns() {
		return this.matrix[0].length;
	}

	public double[][] getData() {
		return this.matrix;
	}

	public void setData(double[][] matrix) {
		if (this.getRows() == matrix.length && this.getColumns() == matrix[0].length) {
			for (int i = 0; i < this.getRows(); i++) {
				for (int j = 0; j < this.getColumns(); j++) {
					this.matrix[i][j] = matrix[i][j];
				}
			}
		} else {
			System.out.println("Matrix dimensions must match");
		}
	}

	public void print() {
		for (int i = 0; i < this.rows; i++) {
			for (int j = 0; j < this.cols; j++) {
				System.out.print((Math.round(this.matrix[i][j] * 100) / 100.0) + "  ");
			}
			System.out.println("");
			System.out.println("");
		}
		System.out.println("---------------------");
		System.out.println("");
	}
}
