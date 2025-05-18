import logging
import datetime as dt
import time,sys,os

def get_logger(name, log_lvl='INFO'):
    log_fmt = '%(asctime)s %(message)s'
    logging.Formatter.converter = time.gmtime

    # Create logger
    logger = logging.getLogger(name)
    logger.setLevel(getattr(logging, log_lvl.upper()))

    # Prevent adding duplicate handlers
    if not logger.handlers:
        formatter = logging.Formatter(log_fmt)

        # Ensure log directory exists
        os.makedirs('../log', exist_ok=True)

        # File handler
        file_handler = logging.FileHandler(f'../log/{name}-log_{dt.date.today()}.txt')
        file_handler.setFormatter(formatter)
        logger.addHandler(file_handler)

        # Stream handler (console)
        stream_handler = logging.StreamHandler(sys.stdout)
        stream_handler.setFormatter(formatter)
        logger.addHandler(stream_handler)

    return logger