(function() {
    if (window['localStorage']) {
        function drawNode(node) {
            if (!empty(node)) {
                var li, dt, c, i, il;

                li = '<li class = "' + node["name"] + '"><span>' + node["name"] + '</span>';
                li += '<input class="add-todo" type="button" value="Add Task"/>';
                li += '<input class="delete-todo" type="button" value="Delete Task"/>';

                // time
                dt = node["datetime"];
                li += '<ul clas="datetime"><span>Clock</span>';
                for (i = 0, il = dt.length; i < il; i++) {
                    li += '<li><input type="text" name="from" value="' + getDateTime(dt[i]["from"]) + '" disabled="disabled"/>';
                    li += '<input type="text" name="to" value="' + getDateTime(dt[i]["to"]) + '" disabled="disabled"/></li>';
                }
                li += '</ul>';

                // drawNode for every child
                c = node["children"];
                li += '<ul class="children"><span>Subtasks</span>';
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
                         "children": [{"id": "0",
                                       "name": "0.0",
                                       "children": [{"id": "0",
                                                     "name": "0.0.0",
                                                     "children": [],
                                                     "datetime": []}],
                                       "datetime": []},
                                      {"id": "1",
                                       "name": "0.1",
                                       "children": [],
                                       "datetime": []},
                                      {"id": "2",
                                       "name": "0.2",
                                       "children": [],
                                       "datetime": []}],
                         "datetime": [{"from": "1324194336718", "to": "1324194631166"},
                                      {"from": "1324194595840", "to": "1324194631166"}]},
                        {"id": "1",
                         "name": "1",
                         "children": [{"id": "0",
                                       "name": "1.0",
                                       "children": [],
                                       "datetime": []},
                                      {"id": "1",
                                       "name": "1.1",
                                       "children": [],
                                       "datetime": []},
                                      {"id": "2",
                                       "name": "1.2",
                                       "children": [],
                                       "datetime": []}],
                         "datetime": [{"from": "1324194336718", "to": "1324194631166"},
                                      {"from": "1324194595840", "to": "1324194631166"}]},
                        {"id": "2",
                         "name": "2",
                         "children": [{"id": "0",
                                       "name": "2.0",
                                       "children": [],
                                       "datetime": []},
                                      {"id": "1",
                                       "name": "2.1",
                                       "children": [],
                                       "datetime": []},
                                      {"id": "2",
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
                todoItems = JSON.parse(todo);
            } else {
                todoItems = getTestTodoList();
            }
            //todoItems = getTestTodoList();storeTodoList();
        }

        function storeTodoList() {
            localStorage.setItem('todoItems', JSON.stringify(todoItems));
        }

        function deleteNode(that) {
            var node, todoNodes, tokens, parent, index;

            // remove node from DOM
            node = that.parentNode;
            node.parentNode.removeChild(node);

            // remove node from todoItems
            tokens = node.className.split('.');
            todoNodes = parent = todoItems;
            for (var i = 0, il = tokens.length; i < il; i++) {
                var jl = todoNodes.length;
                for (var j = 0; j < jl; j++) {
                    if (todoNodes[j].id == tokens[i]) {
                        parent = todoNodes;
                        index = j
                        todoNodes = todoNodes[j].children;
                        break;
                    }
                }
            }
            delete parent[index];

            // store updated todoItems to localStorage
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

            var ulTodoItems = document.querySelector('#todo-items');
            ulTodoItems.parentNode.replaceChild(ul, ulTodoItems);

            addTodoEvents();
        })();
    }
})();
