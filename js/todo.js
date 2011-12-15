(function() {
    if (window['localStorage']) {
        var todoItems = {};
        function populateTodoList() {
            if (todo = localStorage.getItem('todoItems')) {
                todoItems = JSON.parse(todo);
            } else {
                var json = {"1": {"1.1": {}, "1.2": {}, "1.3": {}}, "2": {"2.1": {}, "2.2": {}, "2.3": {}}, "3": {"3.1": {}, "3.2": {}, "3.3": {}}};
                todoItems = json;
            }
        }

        function drawNode(node) {
            if (!empty(node)) {
                console.log(node);
                var l = length(node);
                if (l == 1) {
                    for (var i in node) {
                        console.log(i);
                    }
                } else {
                    var li = '';
                    for (var n in node) {
                        li += '<li><span class = "">' + n + '</span><ul>' + drawNode(node[n]) + '</ul></li>';
                    }
                    console.log(li);
                    return li;
                }
            }
            return '';
        }

        function addTodoEvents() {
            for (var i = 0, s = document.querySelectorAll('.add-todo'), l = s.length; i < l; i++) {
                addEvent(s[i], 'click', function () {
                });
            }
        }

        (function() {
            populateTodoList();
            var ul = document.createElement('ul');
            ul.id = 'todoItems';
            ul.innerHTML = drawNode(todoItems);
            var todoItems = document.querySelector('#todoItems');
            todoItems.parentNode.replaceChild(ul, todoItems);
        })();
    }
})();
