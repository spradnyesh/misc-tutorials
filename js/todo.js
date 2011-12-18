(function() {
    if (window['localStorage']) {
        function drawNode(node) {
            if (!empty(node)) {
                var li, dt, c, i, il;

                li = '<li><span class = "">' + node["name"] + '</span>';
                li += '<input class="add-todo" type="button" value="Add Task"/>';
                li += '<input class="delete-todo" type="button" value="Delete this Task"/>';

                // time
                dt = node["datetime"];
                li += '<ul><span>Clock</span>';
                for (i = 0, il = dt.length; i < il; i++) {
                    li += '<li><input type="text" name="from" value="' + getDateTime(dt[i]["from"]) + '" disabled="disabled"/>';
                    li += '<input type="text" name="to" value="' + getDateTime(dt[i]["to"]) + '" disabled="disabled"/></li>';
                }
                li += '</ul>';

                // drawNode for every child
                c = node["children"];
                li += '<ul><span>Subtasks</span>';
                for (i = 0, il = c.length; i < il; i++) {
                    li += drawNode(c[i]);
                }
                li += '</ul></li>';

                return li;
            }
            return '';
        }

        function getTestTodoList() {
            var json = [{"id": "0",
                         "name": "0",
                         "children": [{"id": "0.0",
                                       "name": "0.0",
                                       "children": [{"id": "0.0.0",
                                                     "name": "0.0.0",
                                                     "children": [],
                                                     "datetime": []}],
                                       "datetime": []},
                                      {"id": "0.1",
                                       "name": "0.1",
                                       "children": [],
                                       "datetime": []},
                                      {"id": "0.2",
                                       "name": "0.2",
                                       "children": [],
                                       "datetime": []}],
                         "datetime": [{"from": "1324194336718", "to": "1324194631166"},
                                      {"from": "1324194595840", "to": "1324194631166"}]},
                        {"id": "1",
                         "name": "1",
                         "children": [{"id": "1.0",
                                       "name": "1.0",
                                       "children": [],
                                       "datetime": []},
                                      {"id": "1.1",
                                       "name": "1.1",
                                       "children": [],
                                       "datetime": []},
                                      {"id": "1.2",
                                       "name": "1.2",
                                       "children": [],
                                       "datetime": []}],
                         "datetime": [{"from": "1324194336718", "to": "1324194631166"},
                                      {"from": "1324194595840", "to": "1324194631166"}]},
                        {"id": "2",
                         "name": "2",
                         "children": [{"id": "2.0",
                                       "name": "2.0",
                                       "children": [],
                                       "datetime": []},
                                      {"id": "2.1",
                                       "name": "2.1",
                                       "children": [],
                                       "datetime": []},
                                      {"id": "2.2",
                                       "name": "2.2",
                                       "children": [],
                                       "datetime": []}],
                         "datetime": [{"from": "1324194336718", "to": "1324194631166"},
                                      {"from": "1324194595840", "to": "1324194631166"}]}];
            return json;
        }

        var todoItems = [];

        function populateTodoList() {
            if (todo = localStorage.getItem('todoItems')) {
                console.log(todo);
                todoItems = JSON.parse(todo);
                console.log(todoItems);
            } else {
                todoItems = getTestTodoList();
            }
            todoItems = getTestTodoList();
        }

        function storeTodoList() {
            console.log(todoItems);
            var s = JSON.stringify(todoItems);
            console.log(s);
            localStorage.setItem('todoItems', s);
        }

        function deleteNode(that) {
            var node = that.parentNode;
            node.parentNode.removeChild(node);

            storeTodoList();
        }

        function addTodoEvents() {
            for (var i = 0, s = document.querySelectorAll('.add-todo'), l = s.length; i < l; i++) {
                addEvent(s[i], 'click', function () {
                });
            }
            for (var i = 0, s = document.querySelectorAll('.delete-todo'), l = s.length; i < l; i++) {
                addEvent(s[i], 'click', function () {
                    deleteNode(this);
                });
            }
        }

        (function() {
            var ul = document.createElement('ul');
            ul.id = 'todo-items';
            ul.innerHTML = '';

            populateTodoList();
            for (var i = 0, il = todoItems.length; i < il; i++) {
                ul.innerHTML += drawNode(todoItems[i]);
            }
            todoItems = document.querySelector('#todo-items');
            todoItems.parentNode.replaceChild(ul, todoItems);

            addTodoEvents();
        })();
    }
})();
