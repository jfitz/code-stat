from flask import Flask, request, render_template

app = Flask(__name__)

@app.route('/')
def hello_world():
  html = render_template('index.jinja.txt', title='CodeStat')
  return html

@app.route('/linecount/', methods=['POST'])
def linecount():
  text = request.form
  return str(text)

if __name__ == '__main__':
  app.run()
