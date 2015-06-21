FROM python:2.7.9
MAINTAINER Tom Willemse <tom@ryuslash.org>

EXPOSE 8000

RUN pip install gunicorn

COPY . /usr/src/hypo

VOLUME /usr/src/hypo/files
WORKDIR /usr/src/hypo

RUN pip install -r requirements.txt

CMD ["gunicorn", "-b", "0.0.0.0:8000", "--access-logfile", "-", "wsgi"]
