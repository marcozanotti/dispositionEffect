import pandas as pd
import numpy as np
from dispositioneffect.utils.difftime import financial_difftime


class Paper:
    """
    Class Paper used to compute paper gains and paper losses based on the chosen method.

    Parameters
    ----------
    df_input : Pandas DataFrame
        Investor's portfolio

    allow_short : bool
        If ``True``, short positions are allowed, otherwise only long positions are allowed.

    method: str, default=``'count'``
        The method used to compute papers. Allowed values are "count", "total", "value", "duration" and "all".

        - When ``'count'``, computation of paper gains and paper losses as simple counts (default method).
        - When ``'total'``, computation of paper gains and paper losses as total quantity of assets.
        - When ``'value'``, computation of paper gains and paper losses as expected return of assets.
        - When ``'duration'``, computation of paper gains and paper losses as financial duration.
        - When ``'all'``, computation of paper gains and paper losses based on ``count``, ``total``, ``value`` and ``duration``.
          Result will be the concatenation of all sub-results.

    """

    def __init__(self, df_input, allow_short, method='count'):
        self.df_input = df_input
        self.allow_short = allow_short
        self.method = method

    def _validate_params(self):
        if self.method not in ['count', 'total', 'value', 'duration', 'all']:
            raise ValueError(
                '`method` should be either count, total, value, duration or all. Got `{}` instead.'.format(self.method)
            )

    def _paper_duration(self, portfolio_quantity, portfolio_price, market_price, transaction_datetime=None,
                        previous_datetime=None, datetime_difference=None):
        """
        Computation of paper gains and paper losses as financial duration.

        Returns
        -------

        """
        price_difference = market_price - portfolio_price
        if transaction_datetime and previous_datetime:
            datetime_difference = financial_difftime(previous_datetime, transaction_datetime)

        if self.allow_short:
            if portfolio_quantity > 0 and price_difference > 0:  # Long - Paper Gain
                res = pd.Series({"RG_duration": 0., "RL_duration": 0,
                                 "PG_duration": datetime_difference, "PL_duration": 0})
            elif portfolio_quantity > 0 > price_difference:  # Long - Paper Loss
                res = pd.Series({"RG_duration": 0., "RL_duration": 0,
                                 "PG_duration": 0, "PL_duration": datetime_difference})
            elif portfolio_quantity < 0 < price_difference:  # Short - Paper Loss
                res = pd.Series({"RG_duration": 0., "RL_duration": 0,
                                 "PG_duration": 0, "PL_duration": datetime_difference})
            elif portfolio_quantity < 0 and price_difference < 0:  # Short - Paper Gain
                res = pd.Series({"RG_duration": 0., "RL_duration": 0,
                                 "PG_duration": datetime_difference, "PL_duration": 0})
            else:  # if portfolio_quantity = 0 or prz_diff = 0
                res = pd.Series({"RG_duration": 0., "RL_duration": 0, "PG_duration": 0, "PL_duration": 0})
        else:
            if portfolio_quantity > 0 and price_difference > 0:  # Long - Paper Gain
                res = pd.Series({"RG_duration": 0., "RL_duration": 0,
                                 "PG_duration": datetime_difference, "PL_duration": 0})
            elif portfolio_quantity > 0 > price_difference:  # Long - Paper Loss
                res = pd.Series({"RG_duration": 0., "RL_duration": 0,
                                 "PG_duration": 0, "PL_duration": datetime_difference})
            else:  # if portfolio_quantity = 0 or prz_diff = 0
                res = pd.Series({"RG_duration": 0., "RL_duration": 0, "PG_duration": 0, "PL_duration": 0})

        return res

    def _paper_count(self, portfolio_quantity, portfolio_price, market_price):
        """
        Computation of paper gains and paper losses as simple counts (default method).

        Returns
        -------

        """
        price_difference = market_price - portfolio_price
        if self.allow_short:
            if portfolio_quantity > 0 and price_difference > 0:  # Long - Paper Gain
                res = pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 1, "PL_count": 0})
            elif portfolio_quantity > 0 > price_difference:  # Long - Paper Loss
                res = pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 0, "PL_count": 1})
            elif portfolio_quantity < 0 < price_difference:  # Short - Paper Loss
                res = pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 0, "PL_count": 1})
            elif portfolio_quantity < 0 and price_difference < 0:  # Short - Paper Gain
                res = pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 1, "PL_count": 0})
            else:  # if portfolio_quantity = 0 or prz_diff = 0
                res = pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 0, "PL_count": 0})
        else:
            if portfolio_quantity > 0 and price_difference > 0:  # Long - Paper Gain
                res = pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 1, "PL_count": 0})
            elif portfolio_quantity > 0 > price_difference:  # Long - Paper Loss
                res = pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 0, "PL_count": 1})
            else:  # if portfolio_quantity = 0 or prz_diff = 0
                res = pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 0, "PL_count": 0})

        return res

    def _paper_total(self, portfolio_quantity, portfolio_price, market_price):
        """
        Computation of paper gains and paper losses as total quantity of assets.

        Returns
        -------

        """
        price_difference = market_price - portfolio_price
        if self.allow_short:
            if portfolio_quantity > 0 and price_difference > 0:  # Long - Paper Gain
                res = pd.Series({"RG_total": 0., "RL_total": 0, "PG_total": portfolio_quantity, "PL_total": 0})
            elif portfolio_quantity > 0 > price_difference:  # Long - Paper Loss
                res = pd.Series({"RG_total": 0., "RL_total": 0, "PG_total": 0, "PL_total": portfolio_quantity})
            elif portfolio_quantity < 0 < price_difference:  # Short - Paper Loss
                res = pd.Series(
                    {"RG_total": 0., "RL_total": 0, "PG_total": 0, "PL_total": np.abs(portfolio_quantity)})
            elif portfolio_quantity < 0 and price_difference < 0:  # Short - Paper Gain
                res = pd.Series(
                    {"RG_total": 0., "RL_total": 0, "PG_total": np.abs(portfolio_quantity), "PL_total": 0})
            else:  # if portfolio_quantity = 0 or prz_diff = 0
                res = pd.Series({"RG_total": 0., "RL_total": 0, "PG_total": 0, "PL_total": 0})
        else:
            if portfolio_quantity > 0 and price_difference > 0:  # Long - Paper Gain
                res = pd.Series({"RG_total": 0., "RL_total": 0, "PG_total": portfolio_quantity, "PL_total": 0})
            elif portfolio_quantity > 0 > price_difference:  # Long - Paper Loss
                res = pd.Series({"RG_total": 0., "RL_total": 0, "PG_total": 0, "PL_total": portfolio_quantity})
            else:  # if portfolio_quantity = 0 or prz_diff = 0
                res = pd.Series({"RG_total": 0., "RL_total": 0, "PG_total": 0, "PL_total": 0})

        return res

    def _paper_value(self, portfolio_quantity, portfolio_price, market_price):
        """
        Computation of paper gains and paper losses as expected return of assets.

        Returns
        -------

        """
        price_difference = market_price - portfolio_price
        expected_return = np.inf if portfolio_price == 0 else price_difference / portfolio_price
        if self.allow_short:
            if portfolio_quantity > 0 and price_difference > 0:  # Long - Paper Gain
                res = pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": expected_return, "PL_value": 0})
            elif portfolio_quantity > 0 > price_difference:  # Long - Paper Loss
                res = pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": 0, "PL_value": expected_return})
            elif portfolio_quantity < 0 < price_difference:  # Short - Paper Loss
                res = pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": 0, "PL_value": -expected_return})
            elif portfolio_quantity < 0 and price_difference < 0:  # Short - Paper Gain
                res = pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": -expected_return, "PL_value": 0})
            else:  # if portfolio_quantity = 0 or prz_diff = 0
                res = pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": 0, "PL_value": 0})
        else:
            if portfolio_quantity > 0 and price_difference > 0:  # Long - Paper Gain
                res = pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": expected_return, "PL_value": 0})
            elif portfolio_quantity > 0 > price_difference:  # Long - Paper Loss
                res = pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": 0, "PL_value": expected_return})
            else:  # if portfolio_quantity = 0 or prz_diff = 0
                res = pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": 0, "PL_value": 0})

        return res

    def compute(self):
        """
        Wrapper that calls internal methods to compute paper gains and paper losses based on the chosen method in Paper.

        Returns
        -------
        res : Pandas Series or Pandas DataFrame, depending on param ``method``

            - If ``method='all'``, the result will be a DataFrame
            - If ``method!='all'``, the result will be a Series

        """
        self._validate_params()

        if self.method == 'value':
            res = self.df_input.apply(lambda row: self._paper_value(row['quantity'], row['price'],
                                                                    row['market_price']), axis=1)
        elif self.method == 'count':
            res = self.df_input.apply(lambda row: self._paper_count(row['quantity'], row['price'],
                                                                    row['market_price']), axis=1)
        elif self.method == 'total':
            res = self.df_input.apply(lambda row: self._paper_total(row['quantity'], row['price'],
                                                                    row['market_price']), axis=1)
        elif self.method == 'duration':
            res = self.df_input.apply(lambda row: self._paper_duration(row['quantity'], row['price'],
                                                                       row['market_price'],
                                                                       row['transaction_dt'],
                                                                       row['previous_dt']
                                                                       ), axis=1)
        else:
            res = pd.concat([
                self.df_input.apply(lambda row: self._paper_value(row['quantity'], row['price'],
                                                                  row['market_price']),
                                    axis=1),
                self.df_input.apply(lambda row: self._paper_count(row['quantity'], row['price'],
                                                                  row['market_price']),
                                    axis=1),
                self.df_input.apply(lambda row: self._paper_total(row['quantity'], row['price'],
                                                                  row['market_price']),
                                    axis=1),
                self.df_input.apply(lambda row: self._paper_duration(row['quantity'], row['price'], row['market_price'],
                                                                     previous_datetime=row['previous_dt'],
                                                                     transaction_datetime=row['transaction_dt']),
                                    axis=1)], axis=1)

        res = pd.concat([res, self.df_input['asset']], axis=1)
        return res
