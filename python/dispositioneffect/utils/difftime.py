import numpy as np
import warnings


def _days_hours_minutes(td):
    return {'days': td.total_seconds()/86400,
            'hours': td.total_seconds()/3600,
            'minutes': td.total_seconds()/60}


def financial_difftime(start_dt, end_dt, pre_market=8, after_market=22, units="hours"):
    """
    Financial implementation of R `difftime <https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/difftime>`_

    Parameters
    ----------
    start_dt: Pandas datetime
        Initial date

    end_dt: Pandas datetime
        Final date

    pre_market: int
        Integer representing the hour of the day at which the pre-market begins

    after_market: int
        Integer representing the hour of the day at which the after-market ends

    units: str
        Character string specifying a time unit

    Returns
    -------
    final: int
        A numeric value in hours

    """
    if start_dt.weekday() in [5, 6] or end_dt.weekday() in [5, 6]:
        warnings.warn("Either start_dt or end_dt is not a valid trading day (weekends)")
        return 0

    upper_start_dt = start_dt.replace(hour=after_market, minute=0, second=0)
    lower_end_dt = end_dt.replace(hour=pre_market, minute=0, second=0)

    if start_dt > upper_start_dt:
        start_dt = upper_start_dt
    if end_dt < lower_end_dt:
        end_dt = lower_end_dt

    if start_dt.date() == end_dt.date():
        final = _days_hours_minutes(end_dt - start_dt)[units]
    else:
        final = (_days_hours_minutes(upper_start_dt - start_dt)[units] +
                 _days_hours_minutes(end_dt - lower_end_dt)[units]) *\
                np.busday_count(start_dt.date(), end_dt.date())
    return final
