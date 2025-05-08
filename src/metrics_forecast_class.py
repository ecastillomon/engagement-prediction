import numpy as np
import pandas as pd
from tqdm import tqdm
from sktime.split import ExpandingWindowSplitter
from sktime.forecasting.model_evaluation import evaluate
from sktime.performance_metrics.forecasting import MeanAbsoluteError,MeanSquaredError,MeanAbsolutePercentageError
from sktime.forecasting.arima import AutoARIMA
from sktime.forecasting.ets import AutoETS
from sktime.forecasting.theta import ThetaForecaster
from sktime.forecasting.auto_reg import AutoREG
from sktime.forecasting.fbprophet import Prophet
from sktime.forecasting.neuralforecast import NeuralForecastLSTM,NeuralForecastRNN
from sktime.forecasting.arima import ARIMA
from function_logger import get_logger

logger=get_logger('metrics_forecast')

models_dict = {
        "AutoARIMA": AutoARIMA(),
        "Theta": ThetaForecaster(sp=1),
        "ETS": AutoETS(sp=1,auto=True,allow_multiplicative_trend=True),
        "Prophet": Prophet(seasonality_mode='multiplicative'),
        "AutoREG(2)": AutoREG(lags=2),
        "AutoREG(3)": AutoREG(lags=3),
        "AutoREG(4)": AutoREG(lags=4),
        "AutoREG(5)": AutoREG(lags=5),
        "AR(2)":ARIMA(order=(2,0,0)),
        "AR(3)":ARIMA(order=(3,0,0)),
        "AR(4)":ARIMA(order=(4,0,0)),
        "AR(5)":ARIMA(order=(5,0,0)),
        "MA(2)":ARIMA(order=(0,0,2)),
        "MA(3)":ARIMA(order=(0,0,3)),
        "MA(4)":ARIMA(order=(0,0,4)),
        "MA(5)":ARIMA(order=(0,0,5)),
        "Neural Forecast LTSM":NeuralForecastLSTM(max_steps=10),
        "Neural Forecast RNN":NeuralForecastRNN(max_steps=10),
    }
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
    def __init__(self, df, date_col='published_date', channel_col='channel_uid'):
        self.df = df.copy()
        self.date_col = date_col
        self.channel_col = channel_col
        self.results = []
        self.trained=[]

    def run(self,metric_cols=None, h=6, initial_window=12, step_length=6,models=['AutoARIMA', 'Theta', 'ETS'],models_dict=models_dict,max_periods=50):
        channels = self.df[self.channel_col].unique()
        ##filter models to be applied
        models_dict = {k: v for k, v in models_dict.items() if k in models}
        total_tasks = len(channels) * len(metric_cols)* len(models_dict.items())
        with tqdm(total=total_tasks, desc="Evaluating models") as pbar:
            for channel in channels:
                logger.info(f"Evaluating channel: {channel}")
                df_channel = self.df[self.df[self.channel_col] == channel].copy()
                df_channel[self.date_col] = pd.to_datetime(df_channel[self.date_col])
                df_channel = df_channel\
                    .sort_values(self.date_col)\
                    .set_index(self.date_col)\
                    .sort_index()\
                    .head(max_periods)
                source=df_channel['source'].iloc[0]
                for metric in metric_cols:
                    logger.info(f"Evaluating metric: {metric}")
                    y = df_channel[metric].dropna()
                    y = y.reset_index(drop=True)
                    if len(y) < initial_window + h:
                        logger.info(f"Skipping {channel} / {metric} (not enough data)")
                        continue
                    for model_name, model in models_dict.items():
                        experiment_name = f"{channel}_{metric}_{model_name}"
                        if experiment_name not in self.trained:
                            logger.info(f"Evaluating {model_name} for {channel}/{metric}")
                            try:
                                evaluator = TimeSeriesEvaluator(y=y, initial_window=initial_window, step_length=step_length, h=h)
                                evaluator.evaluate_sktime_model(model_name, model)
                            except Exception as e:
                                logger.info(f"Failed {model_name} for {channel}/{metric}: {e}")
                            model_results = evaluator.get_results_df().T.reset_index().rename(columns={"index": "model_name"})
                            model_results[self.channel_col] = channel
                            model_results["metric_name"] = metric
                            model_results["source"] = source
                            self.results.append(model_results)
                            self.trained.append(experiment_name)
                        else:
                            logger.info(f"Already evaluated {model_name} for {channel}/{metric}")     
                        pbar.update(1)
                        logger.info(f"""{pbar.n} of {total_tasks}""")

    def get_summary(self):
        return pd.concat(self.results, ignore_index=True)