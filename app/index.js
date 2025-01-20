const request = new XMLHttpRequest();
request.onreadystatechange = function() {
  if (request.readyState === 4 && request.status === 200) {
    const response = JSON.parse(request.responseText);
    // レスポンスを処理するコードを記述
    console.log(response);
  }
};
request.open('GET', '/api/data', true);
request.send();