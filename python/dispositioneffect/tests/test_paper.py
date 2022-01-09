from dispositioneffect.paper import Paper
import pytest as pt
import pandas as pd
import datetime as dt

df_res = pd.DataFrame({'asset': ['A', 'B', 'C', 'D', 'E'],
                       "RG_count": [0., 0, 0, 0, 0],
                       "RL_count": [0., 0, 0, 0, 0],
                       "PG_count": [0., 0, 1, 1, 0],
                       "PL_count": [0., 1, 0, 0, 1],
                       "RG_total": [0., 0, 0, 0, 0],
                       "RL_total": [0., 0, 0, 0, 0],
                       "PG_total": [0., 0, 2, 2, 0],
                       "PL_total": [0., 2, 0, 0, 2],
                       "RG_value": [0., 0, 0, 0, 0],
                       "RL_value": [0., 0, 0, 0, 0],
                       "PG_value": [0., 0, (15 - 10) / 10, -(10 - 15) / 15, 0],
                       "PL_value": [0., (10 - 15) / 15, 0, 0, -(15 - 10) / 10],
                       "RG_duration": [0., 0, 0, 0, 0],
                       "RL_duration": [0., 0, 0, 0, 0],
                       "PG_duration": [0., 0, 14, 14, 0],
                       "PL_duration": [0., 14, 0, 0, 14]}).sort_index(axis=1)

df = pd.DataFrame({'asset': ['A', 'B', 'C', 'D', 'E'],
                   'quantity': [0., 2, 2, -2, -2],
                   'price': [0., 15, 10, 15, 10],
                   'market_price': [0., 10, 15, 10, 15]})
df['previous_dt'] = dt.datetime.strptime('2021-01-04 10:00:00', '%Y-%m-%d %H:%M:%S').astimezone(dt.timezone.utc)
df['transaction_dt'] = dt.datetime.strptime('2021-01-05 10:00:00', '%Y-%m-%d %H:%M:%S').astimezone(dt.timezone.utc)


class TestPaperCount:
    @pt.mark.parametrize(
        'pf_quantity, pf_price, mkt_price, prev_dt, trans_dt, dt_diff, assets, allow_short, method, expected',
        [
            # Zero quantities
            (0., 0, 0, None, None, None, None, True, None,
             pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 0, "PL_count": 0})),
            (0., 15, 10, None, None, None, None, True, None,
             pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 0, "PL_count": 0})),
            (0., 10, 15, None, None, None, None, True, None,
             pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 0, "PL_count": 0})),
            # Long
            (2., 0, 0, None, None, None, None, True, None,
             pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 0, "PL_count": 0})),
            (2., 15, 10, None, None, None, None, True, None,
             pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 0, "PL_count": 1})),
            (2., 10, 15, None, None, None, None, True, None,
             pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 1, "PL_count": 0})),
            # Short
            (-2., 0, 0, None, None, None, None, True, None,
             pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 0, "PL_count": 0})),
            (-2., 15, 10, None, None, None, None, True, None,
             pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 1, "PL_count": 0})),
            (-2., 10, 15, None, None, None, None, True, None,
             pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 0, "PL_count": 1})),
            # allow_short = False
            (2., 0, 0, None, None, None, None, False, None,
             pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 0, "PL_count": 0})),
            (2., 15, 10, None, None, None, None, False, None,
             pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 0, "PL_count": 1})),
            (2., 10, 15, None, None, None, None, False, None,
             pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 1, "PL_count": 0})),
            (-2., 0, 0, None, None, None, None, False, None,
             pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 0, "PL_count": 0})),
            (-2., 15, 10, None, None, None, None, False, None,
             pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 0, "PL_count": 0})),
            (-2., 10, 15, None, None, None, None, False, None,
             pd.Series({"RG_count": 0., "RL_count": 0, "PG_count": 0, "PL_count": 0})),

        ])
    def test_paper_count(self, pf_quantity, pf_price, mkt_price, prev_dt, trans_dt, dt_diff,
                         assets, allow_short, method, expected):
        p = Paper(None, allow_short=allow_short, method=method)
        res = p._paper_count(portfolio_quantity=pf_quantity, portfolio_price=pf_price, market_price=mkt_price)
        assert expected.equals(res)

    @pt.mark.parametrize(
        'pf_quantity, pf_price, mkt_price, prev_dt, trans_dt, dt_diff, assets, allow_short, method, expected',
        [
            # Zero quantities
            (0., 0, 0, None, None, None, None, True, None,
             pd.Series({"RG_total": 0., "RL_total": 0, "PG_total": 0, "PL_total": 0})),
            (0., 15, 10, None, None, None, None, True, None,
             pd.Series({"RG_total": 0., "RL_total": 0, "PG_total": 0, "PL_total": 0})),
            (0., 10, 15, None, None, None, None, True, None,
             pd.Series({"RG_total": 0., "RL_total": 0, "PG_total": 0, "PL_total": 0})),
            # Long
            (2., 0, 0, None, None, None, None, True, None,
             pd.Series({"RG_total": 0., "RL_total": 0, "PG_total": 0, "PL_total": 0})),
            (2., 15, 10, None, None, None, None, True, None,
             pd.Series({"RG_total": 0., "RL_total": 0, "PG_total": 0, "PL_total": 2})),
            (2., 10, 15, None, None, None, None, True, None,
             pd.Series({"RG_total": 0., "RL_total": 0, "PG_total": 2, "PL_total": 0})),
            # Short
            (-2., 0, 0, None, None, None, None, True, None,
             pd.Series({"RG_total": 0., "RL_total": 0, "PG_total": 0, "PL_total": 0})),
            (-2., 15, 10, None, None, None, None, True, None,
             pd.Series({"RG_total": 0., "RL_total": 0, "PG_total": 2, "PL_total": 0})),
            (-2., 10, 15, None, None, None, None, True, None,
             pd.Series({"RG_total": 0., "RL_total": 0, "PG_total": 0, "PL_total": 2})),
            # allow_short = False
            (2., 0, 0, None, None, None, None, False, None,
             pd.Series({"RG_total": 0., "RL_total": 0, "PG_total": 0, "PL_total": 0})),
            (2., 15, 10, None, None, None, None, False, None,
             pd.Series({"RG_total": 0., "RL_total": 0, "PG_total": 0, "PL_total": 2})),
            (2., 10, 15, None, None, None, None, False, None,
             pd.Series({"RG_total": 0., "RL_total": 0, "PG_total": 2, "PL_total": 0})),
            (-2., 0, 0, None, None, None, None, False, None,
             pd.Series({"RG_total": 0., "RL_total": 0, "PG_total": 0, "PL_total": 0})),
            (-2., 15, 10, None, None, None, None, False, None,
             pd.Series({"RG_total": 0., "RL_total": 0, "PG_total": 0, "PL_total": 0})),
            (-2., 10, 15, None, None, None, None, False, None,
             pd.Series({"RG_total": 0., "RL_total": 0, "PG_total": 0, "PL_total": 0})),

        ])
    def test_paper_total(self, pf_quantity, pf_price, mkt_price, prev_dt, trans_dt, dt_diff,
                         assets, allow_short, method, expected):
        p = Paper(None, allow_short=allow_short, method=method)
        res = p._paper_total(portfolio_quantity=pf_quantity, portfolio_price=pf_price, market_price=mkt_price)
        assert expected.equals(res)

    @pt.mark.parametrize(
        'pf_quantity, pf_price, mkt_price, prev_dt, trans_dt, dt_diff, assets, allow_short, method, expected',
        [
            # Zero quantities
            (0., 0, 0, None, None, None, None, True, None,
             pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": 0, "PL_value": 0})),
            (0., 15, 10, None, None, None, None, True, None,
             pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": 0, "PL_value": 0})),
            (0., 10, 15, None, None, None, None, True, None,
             pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": 0, "PL_value": 0})),
            # Long
            (2., 0, 0, None, None, None, None, True, None,
             pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": 0, "PL_value": 0})),
            (2., 15, 10, None, None, None, None, True, None,
             pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": 0, "PL_value": (10 - 15) / 15})),
            (2., 10, 15, None, None, None, None, True, None,
             pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": (15 - 10) / 10, "PL_value": 0})),
            # Short
            (-2., 0, 0, None, None, None, None, True, None,
             pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": 0, "PL_value": 0})),
            (-2., 15, 10, None, None, None, None, True, None,
             pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": -(10 - 15) / 15, "PL_value": 0})),
            (-2., 10, 15, None, None, None, None, True, None,
             pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": 0, "PL_value": -(15 - 10) / 10})),
            # allow_short = False
            (2., 0, 0, None, None, None, None, False, None,
             pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": 0, "PL_value": 0})),
            (2., 15, 10, None, None, None, None, False, None,
             pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": 0, "PL_value": (10 - 15) / 15})),
            (2., 10, 15, None, None, None, None, False, None,
             pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": (15 - 10) / 10, "PL_value": 0})),
            (-2., 0, 0, None, None, None, None, False, None,
             pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": 0, "PL_value": 0})),
            (-2., 15, 10, None, None, None, None, False, None,
             pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": 0, "PL_value": 0})),
            (-2., 10, 15, None, None, None, None, False, None,
             pd.Series({"RG_value": 0., "RL_value": 0, "PG_value": 0, "PL_value": 0})),

        ])
    def test_paper_value(self, pf_quantity, pf_price, mkt_price, prev_dt, trans_dt, dt_diff,
                         assets, allow_short, method, expected):
        p = Paper(None, allow_short=allow_short, method=method)
        res = p._paper_value(portfolio_quantity=pf_quantity, portfolio_price=pf_price, market_price=mkt_price)
        assert expected.equals(res)

    @pt.mark.parametrize(
        'pf_quantity, pf_price, mkt_price, prev_dt, trans_dt, dt_diff, assets, allow_short, method, expected',
        [
            # Zero quantities
            (0., 0, 0, None, None, 5, None, True, None,
             pd.Series({"RG_duration": 0., "RL_duration": 0, "PG_duration": 0, "PL_duration": 0})),
            (0., 15, 10, None, None, 5, None, True, None,
             pd.Series({"RG_duration": 0., "RL_duration": 0, "PG_duration": 0, "PL_duration": 0})),
            (0., 10, 15, None, None, 5, None, True, None,
             pd.Series({"RG_duration": 0., "RL_duration": 0, "PG_duration": 0, "PL_duration": 0})),
            # Long
            (2., 0, 0, None, None, 5, None, True, None,
             pd.Series({"RG_duration": 0., "RL_duration": 0, "PG_duration": 0, "PL_duration": 0})),
            (2., 15, 10, None, None, 5, None, True, None,
             pd.Series({"RG_duration": 0., "RL_duration": 0, "PG_duration": 0, "PL_duration": 5})),
            (2., 10, 15, None, None, 5, None, True, None,
             pd.Series({"RG_duration": 0., "RL_duration": 0, "PG_duration": 5, "PL_duration": 0})),
            # Short
            (-2., 0, 0, None, None, 5, None, True, None,
             pd.Series({"RG_duration": 0., "RL_duration": 0, "PG_duration": 0, "PL_duration": 0})),
            (-2., 15, 10, None, None, 5, None, True, None,
             pd.Series({"RG_duration": 0., "RL_duration": 0, "PG_duration": 5, "PL_duration": 0})),
            (-2., 10, 15, None, None, 5, None, True, None,
             pd.Series({"RG_duration": 0., "RL_duration": 0, "PG_duration": 0, "PL_duration": 5})),
            # allow_short = False
            (2., 0, 0, None, None, 5, None, False, None,
             pd.Series({"RG_duration": 0., "RL_duration": 0, "PG_duration": 0, "PL_duration": 0})),
            (2., 15, 10, None, None, 5, None, False, None,
             pd.Series({"RG_duration": 0., "RL_duration": 0, "PG_duration": 0, "PL_duration": 5})),
            (2., 10, 15, None, None, 5, None, False, None,
             pd.Series({"RG_duration": 0., "RL_duration": 0, "PG_duration": 5, "PL_duration": 0})),
            (-2., 0, 0, None, None, 5, None, False, None,
             pd.Series({"RG_duration": 0., "RL_duration": 0, "PG_duration": 0, "PL_duration": 0})),
            (-2., 15, 10, None, None, 5, None, False, None,
             pd.Series({"RG_duration": 0., "RL_duration": 0, "PG_duration": 0, "PL_duration": 0})),
            (-2., 10, 15, None, None, 5, None, False, None,
             pd.Series({"RG_duration": 0., "RL_duration": 0, "PG_duration": 0, "PL_duration": 0})),

        ])
    def test_paper_duration(self, pf_quantity, pf_price, mkt_price, prev_dt, trans_dt, dt_diff,
                            assets, allow_short, method, expected):
        p = Paper(None, allow_short=allow_short, method=method)
        res = p._paper_duration(portfolio_quantity=pf_quantity, portfolio_price=pf_price,
                                market_price=mkt_price, datetime_difference=dt_diff)
        assert expected.equals(res)

    @pt.mark.parametrize(
        'df_input, allow_short, method, expected',
        [
            # Zero quantities
            (df, True, 'count', df_res.loc[:, df_res.columns.str.contains('asset|count')]),
            (df, True, 'total', df_res.loc[:, df_res.columns.str.contains('asset|total')]),
            (df, True, 'value', df_res.loc[:, df_res.columns.str.contains('asset|value')]),
            (df, True, 'duration', df_res.loc[:, df_res.columns.str.contains('asset|duration')]),
            (df, True, 'all', df_res)
        ])
    def test_paper_compute(self, df_input, allow_short, method, expected):
        p = Paper(df_input=df_input, allow_short=allow_short, method=method)
        res = p.compute().sort_index(axis=1)
        assert expected.equals(res)

    def test_paper_wrong_params(self):
        expected = '`method` should be either count, total, value, duration or all. Got `test` instead.'
        with pt.raises(ValueError) as er:
            Paper(df_input=df, allow_short=True, method='test').compute()
        assert expected == str(er.value)

