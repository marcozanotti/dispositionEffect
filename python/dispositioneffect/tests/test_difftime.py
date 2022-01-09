from dispositioneffect.utils.difftime import financial_difftime
import pytest as pt
import datetime as dt


@pt.mark.parametrize(
    'start_dt, end_dt, units, expected',
    [
        # Same dates
        (dt.datetime.strptime('2021-01-04 09:00:00', '%Y-%m-%d %H:%M:%S'),
         dt.datetime.strptime('2021-01-04 18:00:00', '%Y-%m-%d %H:%M:%S'),
         'hours',
         9.),
        (dt.datetime.strptime('2021-01-04 09:00:00', '%Y-%m-%d %H:%M:%S'),
         dt.datetime.strptime('2021-01-04 18:00:00', '%Y-%m-%d %H:%M:%S'),
         'minutes',
         9. * 60),
        (dt.datetime.strptime('2021-01-04 09:00:00', '%Y-%m-%d %H:%M:%S'),
         dt.datetime.strptime('2021-01-04 18:00:00', '%Y-%m-%d %H:%M:%S'),
         'days',
         9. / 24),
        # Different dates
        (dt.datetime.strptime('2021-01-04 09:00:00', '%Y-%m-%d %H:%M:%S'),
         dt.datetime.strptime('2021-01-05 18:00:00', '%Y-%m-%d %H:%M:%S'),
         'hours',
         23.),
        (dt.datetime.strptime('2021-01-04 09:00:00', '%Y-%m-%d %H:%M:%S'),
         dt.datetime.strptime('2021-01-05 18:00:00', '%Y-%m-%d %H:%M:%S'),
         'minutes',
         23. * 60),
        (dt.datetime.strptime('2021-01-04 09:00:00', '%Y-%m-%d %H:%M:%S'),
         dt.datetime.strptime('2021-01-05 18:00:00', '%Y-%m-%d %H:%M:%S'),
         'days',
         23. / 24),
        # Weekends
        (dt.datetime.strptime('2021-01-08 09:00:00', '%Y-%m-%d %H:%M:%S'),
         dt.datetime.strptime('2021-01-09 18:00:00', '%Y-%m-%d %H:%M:%S'),
         'hours',
         UserWarning),
        (dt.datetime.strptime('2021-01-08 09:00:00', '%Y-%m-%d %H:%M:%S'),
         dt.datetime.strptime('2021-01-09 18:00:00', '%Y-%m-%d %H:%M:%S'),
         'minutes',
         UserWarning),
        (dt.datetime.strptime('2021-01-08 09:00:00', '%Y-%m-%d %H:%M:%S'),
         dt.datetime.strptime('2021-01-09 18:00:00', '%Y-%m-%d %H:%M:%S'),
         'days',
         UserWarning),
    ])
def test_financial_difftime(start_dt, end_dt, units, expected):
    start_dt = start_dt.astimezone(dt.timezone.utc)
    end_dt = end_dt.astimezone(dt.timezone.utc)
    if isinstance(expected, type):
        with pt.warns(UserWarning):
            financial_difftime(start_dt=start_dt, end_dt=end_dt, units=units)
    else:
        res = financial_difftime(start_dt=start_dt, end_dt=end_dt, units=units)
        assert expected == res
