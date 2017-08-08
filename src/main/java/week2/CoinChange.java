package week2;

public class CoinChange {
    int findCombinationsCount(int amount, int coins[]) {
        int result = findCombinationsCount(amount, coins, 0);
        System.out.println(result);
        return result;
    }

    private int findCombinationsCount(int amount, int coins[], int checkFromIndex) {
        if (amount == 0)
            return 1;
        else if (amount < 0 || coins.length == checkFromIndex)
            return 0;
        else {
            int withFirstCoin = findCombinationsCount(amount - coins[checkFromIndex], coins, checkFromIndex);
            int withoutFirstCoin = findCombinationsCount(amount, coins, checkFromIndex + 1);
            return withFirstCoin + withoutFirstCoin;
        }
    }
}
