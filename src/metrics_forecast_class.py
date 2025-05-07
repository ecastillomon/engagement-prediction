import numpy as np
import pandas as pd
from tqdm import tqdm

from sktime.split import ExpandingWindowSplitter
from sktime.forecasting.model_evaluation import evaluate
from sktime.performance_metrics.forecasting import MeanAbsoluteError,MeanSquaredError,MeanAbsolutePercentageError

class TimeSeriesEvaluator:
    def __init__(self, y, initial_window=12, step_length=6, h=6, lags=6):
        self.y_raw = y.copy()
        self.y = y.reset_index(drop=True)  # use positional index
        self.h = h
        self.lags = lags
        self.cv = ExpandingWindowSplitter(
            initial_window=initial_window,
            step_length=step_length,
            fh=np.arange(1, h + 1)
        )

        self.scoring = [
                MeanAbsoluteError(),
                MeanSquaredError(square_root=True),
                MeanAbsolutePercentageError()]
        self.scoring_columns = [
            "test_MeanAbsoluteError",
            "test_MeanAbsolutePercentageError",
            "test_MeanSquaredError"
        ]
        self.results = {}
    def evaluate_sktime_model(self, name, forecaster):
        res = evaluate(
            forecaster=forecaster,
            y=self.y,
            cv=self.cv,
            scoring=self.scoring,
            error_score='raise',
            strategy="refit",
            return_data=True,
            return_model=False
        )
        self.results[name] = res[self.scoring_columns].mean()
    def get_results_df(self):
        return pd.DataFrame(self.results)

class MultiSeriesEvaluator:
    def __init__(self, df, date_col='published_date', channel_col='channel_uid', metric_cols=None):
        self.df = df.copy()
        self.date_col = date_col
        self.channel_col = channel_col
        self.metric_cols = metric_cols or [col for col in df.columns if col not in [date_col, channel_col]]
        self.results = []

    def run(self, h=6, initial_window=12, step_length=6):
        from sktime.forecasting.arima import AutoARIMA
        from sktime.forecasting.ets import AutoETS
        from sktime.forecasting.theta import ThetaForecaster
        from sktime.forecasting.auto_reg import AutoREG
        from sktime.forecasting.fbprophet import Prophet
        from tqdm import tqdm
        channels = self.df[self.channel_col].unique()
        total_tasks = len(channels) * len(self.metric_cols)
        with tqdm(total=total_tasks, desc="Evaluating models") as pbar:
            for channel in channels:
                print(f"Evaluating channel: {channel}")
                df_channel = self.df[self.df[self.channel_col] == channel].copy()
                df_channel[self.date_col] = pd.to_datetime(df_channel[self.date_col])
                df_channel = df_channel.sort_values(self.date_col).set_index(self.date_col).sort_index()
                source=df_channel['source'].iloc[0]
                for metric in self.metric_cols:
                    print(f"Evaluating metric: {metric}")
                    y = df_channel[metric].dropna()
                    y = y.reset_index(drop=True)
                    if len(y) < initial_window + h:
                        print(f"Skipping {channel} / {metric} (not enough data)")
                        continue

                    evaluator = TimeSeriesEvaluator(y=y, initial_window=initial_window, step_length=step_length, h=h)

                    models = {
                        "AutoARIMA": AutoARIMA(),
                        "Theta": ThetaForecaster(sp=1),
                        "ETS": AutoETS(sp=1),
                        "Prophet": Prophet(
                            seasonality_mode='multiplicative',
                            n_changepoints=int(len(y) / 12)
                        ),
                        "AutoREG(2)": AutoREG(lags=2),
                        "AutoREG(3)": AutoREG(lags=3),
                        "AutoREG(4)": AutoREG(lags=4),
                        "AutoREG(5)": AutoREG(lags=5)
                    }
                    

                    for model_name, model in models.items():
                        print(f"Evaluating {model_name} for {channel}/{metric}")
                        try:
                            evaluator.evaluate_sktime_model(model_name, model)
                        except Exception as e:
                            print(f"Failed {model_name} for {channel}/{metric}: {e}")

                    model_results = evaluator.get_results_df().T.reset_index().rename(columns={"index": "model_name"})
                    model_results[self.channel_col] = channel
                    model_results["metric_name"] = metric
                    model_results["source"] = source
                    self.results.append(model_results)
                    pbar.update(1)

    def get_summary(self):
        return pd.concat(self.results, ignore_index=True)