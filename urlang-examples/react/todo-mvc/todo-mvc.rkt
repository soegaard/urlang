#lang at-exp racket
;;
;; Unfinished example
;;

;; (require urlang urlang/for urlang/extra urlang/html)
;; (require "../jsx.rkt")

;; (current-urlang-echo?     #t)
;; (current-urlang-beautify? #t)
;; (current-urlang-run?      #f)

;; (urlang
;;  (urmodule todo-mvc
;;    (import this 
;;            React classNames
;;            ReactDOM
;;            $
;;            localStorage JSON)
;;    (var [ESCAPE_KEY 27]
;;         [ENTER_KEY  13])
;;    (define TodoItem
;;      (React.createClass
;;          (object
;;           [getInitialState
;;            (λ () (object [editText this.props.todo.title]))]
;;           [handleSubmit
;;            (λ (event)
;;              (var [val (this.state.editText.trim)])
;;              (cond
;;                [(= val "") (this.props.onDestroy)]
;;                [else       (this.props.onSave val)
;;                            (this.setState (object [editText val]))]))]
;;           [handleEdit
;;            (λ ()
;;              (this.props.onEdit)
;;              (this.setState (object [editText this.props.todo.title])))]
;;           [handleKeyDown
;;            (λ (event)
;;              (cond
;;                [(= event.which ESCAPE_KEY)
;;                 (this.setState (object [editText this.props.todo.title]))
;;                 (this.props.onCancel event)]
;;                [(= event.which ENTER_KEY)
;;                 (this.handleSubmit event)]))]
;;           [handleChange
;;            (λ (event)
;;              (when this.props.editing
;;                (this.setState (object [editText event.target.value]))))]
;;           ; TODO [shouldComponentUpdate ...]
;;           ; TODO [componentDidUpdate ...]
;;           [render
;;            (λ ()
;;              @jsx{@li[className: @ur[(classNames (object [completed this.props.todo.completed]
;;                                                          [editing   this.props.editing]))]]{
;;                   @div[className: "view"]{
;;                      @input[className: "toggle"
;;                             type:      "checkbox"
;;                             checked:   @ur[this.props.todo.completed]
;;                             onChange:  @ur[this.props.onToggle]]}}})])))

;;    ;;;
;;    ;;; UTILS
;;    ;;;
;;    (var [Utils (object [store (λ (namespace data)
;;                                 (when data
;;                                   (localStorage.setItem namespace (JSON.stringify data)))
;;                                 (var [store (localStorage.getItem namespace)])
;;                                 (or (and store (JSON.parse store)) (array)))])])

   
;;    ;; Generic "model" object. You can use whatever
;;    ;; framework you want. For this application it
;;    ;; may not even be worth separating this logic
;;    ;; out, but we do this to demonstrate one way to
;;    ;; separate out parts of your application.
;;    (var app)
;;    (:= app (object [TodoModel (λ (key)
;;                                 (:= this "key"       key)
;;                                 (:= this "todos"     (Utils.store key))
;;                                 (:= this "onChanges" (array)))]))   
;;    (var [proto app.TodoModel.prototype])
;;    (:= proto "subscribe" (λ (onChange) (this.onChanges.push onChange)))
;;    (:= proto "inform"    (λ ()         (Utils.store this.key this.todos)
;;                                        (this.onChanges.forEach (λ (cb) (cb)))))
;;    (:= proto "addTodo"   (λ (title)    (:= this "todos" (this.todos.concat
;;                                                          (object [id        (Utils.uuid)]
;;                                                                  [title     title]
;;                                                                  [completed #f])))
;;                                        (this.inform)))
;;    (:= proto "toggleAll" (λ (checked)
;;                            ;; Note: it's usually better to use immutable data structures since they're
;;                            ;; easier to reason about and React works very well with them. That's why
;;                            ;; we use map() and filter() everywhere instead of mutating the array or
;;                            ;; todo items themselves.
;;                            (:= this.todos
;;                                (this.todos.map
;;                                 (λ (todo)
;;                                   (Utils.extend (object) todo (object [completed checked]))))
;;             (this.inform))))
;;    (:= proto "toggle"
;;        (λ (todoToToggle) 
;;          (:= this "todos" (this.todos.map (λ (todo)
;;                                           (if (= todo todoToToggle)
;;                                               (Utils.extend (object) todo
;;                                                 (object [completed (not todo.completed)]))
;;                                               todo))))
;;          (this.inform)))
;;    (:= proto "destroy"
;;        (λ (todo) (:= this "todos" (this.todos.filter (λ (candidate) (not (= candidate todo)))))
;;                  (this.inform)))
;;    (:= proto "save"
;;        (λ (todoToSave text)
;;          (:= this "todos"
;;              (this.todos.map (λ (todo)
;;                                (if (= todo todoToSave)
;;                                    (Utils.extend (object) todo (object [title text]))
;;                                    todo))))
;;          (this.inform)))
;;    (:= proto "clearCompleted"
;;        (λ () 
;;          (:= this "todos" (this.todos.filter (λ (todo) (not todo.completed))))
;;          (this.inform)))
;;    ;;;
;;    ;;; APP
;;    ;;;

;;    (:= app "ALL_TODOS"       "all")
;;    (:= app "ACTIVE_TODOS"    "active")
;;    (:= app "COMPLETED_TODOS" "completed")
;;    (var [TodoFooter app.TodoFooter])
;;    (var [TodoItem   app.TodoItem])
;;    (var
;;     [TodoApp
;;      (React.createClass
;;          (object
;;           [getInitialState (λ ()
;;                              (object [nowShowing app.ALL_TODOS]
;;                                      [editing    null]
;;                                      [newTodo    ""]))]

;;           [componentDidMount
;;            (λ ()
;;              (var [setState this.setState])
;;              (var [router
;;                    (Router
;;                        (object
;;                         ["/"          (setState.bind this (object [nowShowing app.ALL_TODOS]))]
;;                         ["/active"    (setState.bind this (object [nowShowing app.ACTIVE_TODOS]))]
;;                         ["/completed" (setState.bind this (object [nowShowing app.COMPLETED_TODOS]))]))])
;;              (router.init "/"))]
;;           [handleChange (λ (event) (this.setState (object [newTodo event.target.value])))]
;;           [handleNewTodoKeyDown (λ (event) 
;;                                   (unless (= event.keyCode ENTER_KEY)
;;                                     (event.preventDefault)
;;                                     (let ([val this.state.newTodo.trim])
;;                                       (when val
;;                                         (this.props.model.addTodo val)
;;                                         (this.setState (object [newTodo ""]))))))]
;;           [toggleAll      (λ (event) 
;;                             (var [checked event.target.checked])
;;                             (this.props.model.toggleAll checked))]
          
;;           [toggle         (λ (todoToToggle) (this.props.model.toggle todoToToggle))]
;;           [destroy        (λ (todo) (this.props.model.destroy todo))]
;;           [edit           (λ (todo) (this.setState (object [editing todo.id])))]
;;           [save           (λ (todoToSave text)
;;                             (this.props.model.save todoToSave text)
;;                             (this.setState (object [editing null])))]
;;           [cancel         (λ () (this.setState (object [editing null])))]
;;           [clearCompleted (λ () (this.props.model.clearCompleted))]
;;           (render
;;            (λ ()
;;              (var footer main)
;;              (var [todos this.props.model.todos])
;;              (var [shownTodos (todos.filter
;;                                (λ (todo)
;;                                  (let ([now (this.state.nowShowing)])
;;                                  (cond [(= now app.ACTIVE_TODOS)
;; 					(not todo.completed)
;;                                         [(= now app.COMPLETED_TODOS)
;;                                          todo.completed]
;;                                         [else
;;                                          #t]])))
;;                                this)])

;;              HERE
             
;;              var todoItems = shownTodos.map(function (todo) {
;; 				return (
;; 					<TodoItem
;; 						key={todo.id}
;; 						todo={todo}
;; 						onToggle={this.toggle.bind(this, todo)}
;; 						onDestroy={this.destroy.bind(this, todo)}
;; 						onEdit={this.edit.bind(this, todo)}
;; 						editing={this.state.editing === todo.id}
;; 						onSave={this.save.bind(this, todo)}
;; 						onCancel={this.cancel}
;; 					/>
;; 				);
;; 			}, this);

;; 			var activeTodoCount = todos.reduce(function (accum, todo) {
;; 				return todo.completed ? accum : accum + 1;
;; 			}, 0);

;; 			var completedCount = todos.length - activeTodoCount;

;; 			if (activeTodoCount || completedCount) {
;; 				footer =
;; 					<TodoFooter
;; 						count={activeTodoCount}
;; 						completedCount={completedCount}
;; 						nowShowing={this.state.nowShowing}
;; 						onClearCompleted={this.clearCompleted}
;; 					/>;
;; 			}

;; 			if (todos.length) {
;; 				main = (
;; 					<section className="main">
;; 						<input
;; 							className="toggle-all"
;; 							type="checkbox"
;; 							onChange={this.toggleAll}
;; 							checked={activeTodoCount === 0}
;; 						/>
;; 						<ul className="todo-list">
;; 							{todoItems}
;; 						</ul>
;; 					</section>
;; 				);
;; 			}

;; 			return (
;; 				<div>
;; 					<header className="header">
;; 						<h1>todos</h1>
;; 						<input
;; 							className="new-todo"
;; 							placeholder="What needs to be done?"
;; 							value={this.state.newTodo}
;; 							onKeyDown={this.handleNewTodoKeyDown}
;; 							onChange={this.handleChange}
;; 							autoFocus={true}
;; 						/>
;; 					</header>
;; 					{main}
;; 					{footer}
;; 				</div>
;; 			);
;; 		}
;; 	});

;; 	var model = new app.TodoModel('react-todos');

;; 	function render() {
;; 		React.render(
;; 			<TodoApp model={model}/>,
;; 			document.getElementsByClassName('todoapp')[0]
;; 		);
;; 	}

;; 	model.subscribe(render);
;; 	render();
;; })();
   

;;    ;;;
;;    ;;; START
;;    ;;;
   
;;    (define ($0 selector) (ref ($ selector) 0))
;;    (ReactDOM.render (React.createElement TodoItem) ($0 "#todoapp2"))
;;    ))

;; ;@comment[@html:label[onDoubleClick: @ur[this.handleEdit]]{@ur[this.props.todo.title]}]
;; ;@button[className: "destroy" onClick: @ur[this.props.onDestroy]]}
;; ;                     @input[ref:       "editField"
;; ;                            className: "edit"
;; ;                            value:     @ur[this.state.editText]
;; ;                            onBlur:    @ur[this.handleSubmit]
;; ;                            onChange:  @ur[this.handleChange]
;; ;                            onKeyDown: @ur[this.handleKeyDown]]}})])))
