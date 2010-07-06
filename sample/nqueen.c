print_board(int *board) {
    int i;
    int j;
    for (i = 0; i < 8; i = i + 1) {
        for (j = 0; j < 8; j = j + 1) {
            if (board[i * 8 + j]) {
                printf("Q ");
            } else {
                printf(". ");
            }
        }
        printf("\n");
    }
}

safe(int *board, int row, int col) {
    int i;
    for (i = 0; i < row; i = i + 1) {
        if (board[i * 8 + col])
            return 0;
        int j = row - i;
        if (0 <= (col - j))
            if (board[i * 8 + col - j])
                return 0;
        if ((col + j) < 8)
            if (board[i * 8 + col + j])
                return 0;
    }
    return 1;
}

solve(int *board, int row) {
    if (row == 8) {
        print_board(board);
        printf("\n\n");
        return;
    }
    int i;
    for (i = 0; i < 8; i = i + 1) {
        if (safe(board, row, i)) {
            board[row * 8 + i] = 1;
            solve(board, row + 1);
        }
        board[row * 8 + i] = 0;
    }
}

main() {
    int board[64];
    int i;
    for (i = 0; i < 64; i = i + 1) {
        board[i] = 0;
    }
    solve(board, 0);
    return 0;
}
